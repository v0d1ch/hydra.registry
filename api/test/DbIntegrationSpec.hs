module DbIntegrationSpec (spec) where

import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Db qualified
import Db.Schema (Head (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraUtxoEntry (..))
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "Db (integration)" $ around withTestPool $ do
  describe "initDb" $ do
    it "creates tables without error (idempotent)" $ \pool -> do
      Db.initDb pool

  describe "upsertHead" $ do
    it "inserts a new head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found after insert"
        Just Head{headId, headHost, headStatus} -> do
          headId `shouldBe` ("head-1" :: Text)
          headHost `shouldBe` ("localhost" :: Text)
          headStatus `shouldBe` ("Open" :: Text)

    it "updates existing head on conflict" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.upsertHead pool "head-1" "localhost" 4001 "Closed"
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found after upsert"
        Just Head{headId} ->
          headId `shouldBe` ("head-1" :: Text)

  describe "updateHeadStatus" $ do
    it "changes status of an existing head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.updateHeadStatus pool "head-1" "Closed"
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found"
        Just Head{headStatus} ->
          headStatus `shouldBe` ("Closed" :: Text)

  describe "getAllHeads" $ do
    it "returns all registered heads" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.upsertHead pool "head-2" "localhost" 4002 "Closed"
      heads <- Db.getAllHeads pool
      length heads `shouldBe` 2

  describe "getAllHeadsPaginated" $ do
    it "returns paginated results" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.upsertHead pool "head-2" "localhost" 4002 "Open"
      Db.upsertHead pool "head-3" "localhost" 4003 "Open"
      page1 <- Db.getAllHeadsPaginated pool 2 1
      length page1 `shouldBe` 2
      page2 <- Db.getAllHeadsPaginated pool 2 2
      length page2 `shouldBe` 1

  describe "getHead" $ do
    it "returns Nothing for non-existent head" $ \pool -> do
      mHead <- Db.getHead pool "non-existent"
      mHead `shouldBe` Nothing

    it "returns the head when it exists" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Expected head to exist"
        Just _ -> pure ()

  describe "replaceUtxos" $ do
    it "stores UTxOs for a head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 1

    it "replaces existing UTxOs" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      let newEntry = sampleUtxoEntry{txHash = "newtx123", lovelace = 10_000_000}
      Db.replaceUtxos pool "head-1" [newEntry]
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 1

    it "handles empty UTxO list" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.replaceUtxos pool "head-1" []
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 0

  describe "getUtxosByAddressFlat" $ do
    it "returns UTxOs from all heads" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.upsertHead pool "head-2" "localhost" 4002 "Open"
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.replaceUtxos pool "head-2" [sampleUtxoEntry{txHash = "other-tx"}]
      results <- Db.getUtxosByAddressFlat pool "addr1qxtest" 100 1
      length results `shouldBe` 2

  describe "deleteUtxosForHead" $ do
    it "removes all UTxOs for a head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.deleteUtxosForHead pool "head-1"
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 0

  describe "deleteHead" $ do
    it "removes head and its UTxOs (cascade)" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.deleteHead pool "head-1"
      mHead <- Db.getHead pool "head-1"
      mHead `shouldBe` Nothing
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 0

  describe "countUtxosForHead" $ do
    it "counts UTxOs correctly" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      let entries =
            [ sampleUtxoEntry
            , sampleUtxoEntry{txHash = "tx2", outputIndex = 1}
            , sampleUtxoEntry{txHash = "tx3", outputIndex = 0, address = "addr1other"}
            ]
      Db.replaceUtxos pool "head-1" entries
      count <- Db.countUtxosForHead pool "head-1"
      count `shouldBe` 3

  describe "getAddressesForHead" $ do
    it "returns distinct addresses" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      let entries =
            [ sampleUtxoEntry
            , sampleUtxoEntry{txHash = "tx2", outputIndex = 1}
            , sampleUtxoEntry{txHash = "tx3", address = "addr1other"}
            ]
      Db.replaceUtxos pool "head-1" entries
      addrs <- Db.getAddressesForHead pool "head-1"
      length addrs `shouldBe` 2

  describe "getBalanceForAddressInHead" $ do
    it "aggregates lovelace across UTxOs" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"
      let entries =
            [ sampleUtxoEntry{lovelace = 3_000_000}
            , sampleUtxoEntry{txHash = "tx2", outputIndex = 1, lovelace = 7_000_000}
            ]
      Db.replaceUtxos pool "head-1" entries
      (total, _) <- Db.getBalanceForAddressInHead pool "head-1" "addr1qxtest"
      total `shouldBe` (10_000_000 :: Int64)

  describe "checkDbConnectivity" $ do
    it "returns True when DB is reachable" $ \pool -> do
      result <- Db.checkDbConnectivity pool
      result `shouldBe` True
