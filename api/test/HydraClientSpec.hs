module HydraClientSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Hydra.Client
import Hydra.Submit (SubmitResult (..), classify)
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "Hydra.Client" $ do
  describe "Hydra.Submit.classify" $ do
    it "parses TxValid with transactionId at top level" $ do
      let msg = Aeson.object
            [ "tag"           Aeson..= ("TxValid" :: String)
            , "headId"        Aeson..= ("head-abc" :: String)
            , "transactionId" Aeson..= ("deadbeef" :: String)
            ]
      classify msg `shouldBe` Just SubmitValid{txId = "deadbeef"}

    it "returns Nothing for transaction.txId nested shape (wrong shape)" $ do
      let msg = Aeson.object
            [ "tag"         Aeson..= ("TxValid" :: String)
            , "transaction" Aeson..= Aeson.object ["txId" Aeson..= ("deadbeef" :: String)]
            ]
      classify msg `shouldBe` Just SubmitValid{txId = ""}

    it "parses TxInvalid reason" $ do
      let msg = Aeson.object
            [ "tag"             Aeson..= ("TxInvalid" :: String)
            , "validationError" Aeson..= Aeson.object ["reason" Aeson..= ("script failed" :: String)]
            ]
      classify msg `shouldBe` Just SubmitInvalid{reason = "script failed"}

    it "returns Nothing for unrelated messages" $ do
      let msg = Aeson.object [("tag", "SnapshotConfirmed")]
      classify msg `shouldBe` Nothing

  describe "normalizeHost" $ do
    it "rewrites 'localhost' to '127.0.0.1' to force IPv4" $ do
      -- Linux glibc resolves 'localhost' to ::1 first; hydra-node binds
      -- IPv4 only, so the IPv6 connect attempt fails with a confusing
      -- "does not exist" error before the resolver retries IPv4.
      normalizeHost "localhost" `shouldBe` "127.0.0.1"

    it "passes other hostnames through unchanged" $ do
      normalizeHost "127.0.0.1" `shouldBe` "127.0.0.1"
      normalizeHost "myhost.example.com" `shouldBe` "myhost.example.com"
      normalizeHost "node-2.preview.iohk.io" `shouldBe` "node-2.preview.iohk.io"

  describe "parseHydraMessage" $ do
    it "parses Greetings message with headId and status" $ do
      let msg = mkGreetingsJson "head-abc" "Open" []
      parseHydraMessage msg
        `shouldBe` Just
          HeadGreetings
            { greeterHeadId = "head-abc"
            , greeterHeadStatus = "Open"
            , greeterUtxos = []
            , greeterParticipants = []
            , greeterCurrentSlot = 0
            }

    it "parses Greetings with empty headId (Idle state)" $ do
      let msg =
            Object $
              KM.fromList
                [ ("tag", String "Greetings")
                , ("headStatus", String "Idle")
                ]
      let result = parseHydraMessage msg
      case result of
        Just HeadGreetings{greeterHeadId, greeterHeadStatus} -> do
          greeterHeadId `shouldBe` ""
          greeterHeadStatus `shouldBe` "Idle"
        _ -> expectationFailure "Expected HeadGreetings"

    it "parses Greetings with UTxOs" $ do
      let utxo = mkTxOutJson "addr1qxtest" 5_000_000
          msg = mkGreetingsJson "head-abc" "Open" [("txhash123#0", utxo)]
      case parseHydraMessage msg of
        Just HeadGreetings{greeterUtxos} -> do
          length greeterUtxos `shouldBe` 1
          let entry = head greeterUtxos
          entry.txHash `shouldBe` "txhash123"
          entry.outputIndex `shouldBe` 0
          entry.address `shouldBe` "addr1qxtest"
          entry.lovelace `shouldBe` 5_000_000
        _ -> expectationFailure "Expected HeadGreetings with UTxOs"

    it "parses Greetings with native assets" $ do
      let utxo = mkTxOutWithAssetsJson "addr1qxtest" 2_000_000 [("policy1", [("token1", 100)])]
          msg = mkGreetingsJson "head-abc" "Open" [("txhash123#0", utxo)]
      case parseHydraMessage msg of
        Just HeadGreetings{greeterUtxos} -> do
          let entry = head greeterUtxos
          entry.nativeAssets `shouldBe` Map.fromList [("policy1", Map.fromList [("token1", 100)])]
        _ -> expectationFailure "Expected HeadGreetings with native assets"

    it "parses SnapshotConfirmed message" $ do
      let utxo = mkTxOutJson "addr1qxtest" 10_000_000
          msg = mkSnapshotConfirmedJson "head-xyz" [("txhash456#1", utxo)]
      case parseHydraMessage msg of
        Just HeadSnapshotConfirmed{snapHeadId, snapUtxos} -> do
          snapHeadId `shouldBe` "head-xyz"
          length snapUtxos `shouldBe` 1
          (head snapUtxos).outputIndex `shouldBe` 1
        _ -> expectationFailure "Expected HeadSnapshotConfirmed"

    -- Regression for the manual-e2e finding on 2026-04-30: the
    -- snapshot for an incremental commit reports an empty @utxo@
    -- and the deposit lives in @utxoToCommit@. Earlier the parser
    -- only read @utxo@ and so the registry never saw the deposited
    -- UTxO arrive.
    it "merges utxoToCommit into snapUtxos for incremental commits" $ do
      let depositUtxo = mkTxOutJson "addr1deposit" 30_000_000
          msg =
            mkSnapshotConfirmedWithCommitJson
              "head-xyz"
              [] -- empty utxo
              [("deposit-tx#1", depositUtxo)] -- pending commit
      case parseHydraMessage msg of
        Just HeadSnapshotConfirmed{snapUtxos} -> do
          length snapUtxos `shouldBe` 1
          (head snapUtxos).address `shouldBe` "addr1deposit"
          (head snapUtxos).lovelace `shouldBe` 30_000_000
        _ -> expectationFailure "Expected HeadSnapshotConfirmed"

    it "merges both utxo and utxoToCommit when both are non-empty" $ do
      let existing = mkTxOutJson "addr1existing" 5_000_000
          deposit = mkTxOutJson "addr1deposit" 30_000_000
          msg =
            mkSnapshotConfirmedWithCommitJson
              "head-xyz"
              [("existing-tx#0", existing)]
              [("deposit-tx#1", deposit)]
      case parseHydraMessage msg of
        Just HeadSnapshotConfirmed{snapUtxos} ->
          length snapUtxos `shouldBe` 2
        _ -> expectationFailure "Expected HeadSnapshotConfirmed"

    it "parses HeadIsClosed message" $ do
      let msg = mkHeadIsClosedJson "head-closed"
      parseHydraMessage msg
        `shouldBe` Just (HeadClosed "head-closed")

    it "parses HeadIsFinalized message" $ do
      let msg = mkHeadIsFinalizedJson "head-final" []
      parseHydraMessage msg
        `shouldBe` Just
          HeadFinalized
            { finalizedHeadId = "head-final"
            , finalizedUtxos = []
            }

    it "returns Nothing for unknown tags" $ do
      let msg =
            Object $
              KM.fromList
                [ ("tag", String "UnknownTag")
                , ("headId", String "head-abc")
                ]
      parseHydraMessage msg `shouldBe` Nothing

    it "returns Nothing for non-object values" $ do
      parseHydraMessage (String "not an object") `shouldBe` Nothing
      parseHydraMessage (Number 42) `shouldBe` Nothing
      parseHydraMessage Null `shouldBe` Nothing

    it "returns Nothing when headId is missing from SnapshotConfirmed" $ do
      let msg =
            Object $
              KM.fromList
                [ ("tag", String "SnapshotConfirmed")
                ]
      parseHydraMessage msg `shouldBe` Nothing

  describe "parseUtxoMap" $ do
    it "parses an empty UTxO map" $ do
      parseUtxoMap (Object KM.empty) `shouldBe` []

    it "parses multiple UTxO entries" $ do
      let utxoMap =
            Object $
              KM.fromList
                [ ("tx1#0", mkTxOutJson "addr1" 1_000_000)
                , ("tx2#1", mkTxOutJson "addr2" 2_000_000)
                ]
      let entries = parseUtxoMap utxoMap
      length entries `shouldBe` 2

    it "skips entries with invalid key format" $ do
      let utxoMap =
            Object $
              KM.fromList
                [ ("invalidkey", mkTxOutJson "addr1" 1_000_000)
                , ("tx1#0", mkTxOutJson "addr2" 2_000_000)
                ]
      let entries = parseUtxoMap utxoMap
      length entries `shouldBe` 1

    it "returns empty for non-object value" $ do
      parseUtxoMap (String "not an object") `shouldBe` []
      parseUtxoMap Null `shouldBe` []

    it "parses datum hash and inline datum" $ do
      let txOut =
            Object $
              KM.fromList
                [ ("address", String "addr1qxtest")
                , ("value", Object $ KM.fromList [("lovelace", Number 1_000_000)])
                , ("datumhash", String "abc123")
                , ("inlineDatum", Object $ KM.fromList [("int", Number 42)])
                ]
          utxoMap = Object $ KM.fromList [("tx1#0", txOut)]
      let entries = parseUtxoMap utxoMap
      length entries `shouldBe` 1
      let entry = head entries
      entry.datumHash `shouldBe` Just "abc123"
      entry.inlineDatum `shouldBe` Just (Object $ KM.fromList [("int", Number 42)])
