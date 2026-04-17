module ApiTypesSpec (spec) where

import Api (utxoToResponse)
import Api.Types
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson qualified as Aeson
import Data.Time (getCurrentTime)
import Db.Schema (Utxo (..))
import Test.Hspec

spec :: Spec
spec = describe "Api.Types" $ do
  describe "JSON roundtrips" $ do
    it "RegisterHead roundtrips" $ do
      let val = RegisterHead "localhost" 4001
      roundtrip val `shouldBe` Just val

    it "RegisterHeadResponse roundtrips" $ do
      let val = RegisterHeadResponse "head-abc" "connected"
      roundtrip val `shouldBe` Just val

    it "HeadInfo roundtrips" $ do
      let val = HeadInfo "head-abc" "localhost" 4001 "Open"
      roundtrip val `shouldBe` Just val

    it "EnrichedHeadDetail roundtrips" $ do
      now <- getCurrentTime
      let val =
            EnrichedHeadDetail
              { headId = "head-abc"
              , host = "localhost"
              , port = 4001
              , status = "Open"
              , utxoCount = 5
              , registeredAt = now
              , lastSeenAt = Just now
              , onChain = Nothing
              }
      roundtrip val `shouldBe` Just val

    it "Amount roundtrips" $ do
      let val = Amount "lovelace" "5000000"
      roundtrip val `shouldBe` Just val

    it "UtxoResponse roundtrips" $ do
      let val =
            UtxoResponse
              { address = "addr1qxtest"
              , tx_hash = "abc123"
              , output_index = 0
              , amount = [Amount "lovelace" "5000000"]
              , data_hash = Nothing
              , inline_datum = Nothing
              , reference_script_hash = Nothing
              , head_id = "head-abc"
              }
      roundtrip val `shouldBe` Just val

    it "HeadUtxoResponse roundtrips" $ do
      let val =
            HeadUtxoResponse
              { head_id = "head-abc"
              , head_status = "Open"
              , utxos = []
              }
      roundtrip val `shouldBe` Just val

    it "BalanceResponse roundtrips" $ do
      let val =
            BalanceResponse
              { address = "addr1qxtest"
              , headId = "head-abc"
              , lovelace = "5000000"
              , tokens = [Amount "policy1token1" "100"]
              }
      roundtrip val `shouldBe` Just val

    it "HealthResponse roundtrips" $ do
      let val = HealthResponse "ok" 3 True
      roundtrip val `shouldBe` Just val

    it "RootResponse roundtrips" $ do
      let val = RootResponse "0.1.0" "test"
      roundtrip val `shouldBe` Just val

    it "ErrorResponse roundtrips" $ do
      let val = ErrorResponse "something went wrong"
      roundtrip val `shouldBe` Just val

    it "ExplorerHeadInfo roundtrips" $ do
      now <- getCurrentTime
      let val =
            ExplorerHeadInfo
              { headId = "abc123"
              , network = "Mainnet"
              , networkMagic = 764824073
              , version = "0.21.0"
              , status = "Open"
              , contestationPeriod = Just 60
              , contestations = Nothing
              , snapshotNumber = Just 5
              , contestationDeadline = Nothing
              , point = Nothing
              , blockNo = Just 12345
              , members = Nothing
              , seedTxIn = Just "tx123#0"
              , firstSeenAt = now
              , lastUpdatedAt = now
              , registered = True
              }
      roundtrip val `shouldBe` Just val

    it "ExplorerHeadOnChain roundtrips" $ do
      let val =
            ExplorerHeadOnChain
              { network = "Mainnet"
              , onChainStatus = "Open"
              , contestationPeriod = Just 60
              , contestations = Nothing
              , snapshotNumber = Just 5
              , contestationDeadline = Nothing
              , members = Nothing
              , seedTxIn = Just "tx123#0"
              , blockNo = Just 12345
              }
      roundtrip val `shouldBe` Just val

    it "EnrichedHeadDetail with onChain roundtrips" $ do
      now <- getCurrentTime
      let onChain =
            ExplorerHeadOnChain
              { network = "Mainnet"
              , onChainStatus = "Closed"
              , contestationPeriod = Just 60
              , contestations = Just 1
              , snapshotNumber = Just 10
              , contestationDeadline = Just "2025-01-01T00:00:00Z"
              , members = Nothing
              , seedTxIn = Nothing
              , blockNo = Nothing
              }
          val =
            EnrichedHeadDetail
              { headId = "head-abc"
              , host = "localhost"
              , port = 4001
              , status = "Open"
              , utxoCount = 5
              , registeredAt = now
              , lastSeenAt = Just now
              , onChain = Just onChain
              }
      roundtrip val `shouldBe` Just val

    it "StatsResponse with explorerHeadCount roundtrips" $ do
      let val =
            StatsResponse
              { headCount = 3
              , totalUtxos = 100
              , headsByStatus = mempty
              , explorerHeadCount = 42
              }
      roundtrip val `shouldBe` Just val

  describe "utxoToResponse" $ do
    it "converts a basic UTxO with lovelace only" $ do
      now <- getCurrentTime
      let utxo =
            Utxo
              { utxoTxHash = "txhash123"
              , utxoOutputIndex = 0
              , utxoHeadId = "head-abc"
              , utxoAddress = "addr1qxtest"
              , utxoLovelace = 5_000_000
              , utxoAssets = Aeson.object []
              , utxoDatumHash = Nothing
              , utxoInlineDatum = Nothing
              , utxoReferenceScriptHash = Nothing
              , utxoUpdatedAt = now
              }
      let resp = utxoToResponse utxo.utxoHeadId utxo
      resp.address `shouldBe` "addr1qxtest"
      resp.tx_hash `shouldBe` "txhash123"
      resp.output_index `shouldBe` 0
      length resp.amount `shouldBe` 1
      (head resp.amount).unit `shouldBe` "lovelace"
      (head resp.amount).quantity `shouldBe` "5000000"

    it "converts a UTxO with native assets" $ do
      now <- getCurrentTime
      let assets =
            Aeson.object
              [ "policy1" Aeson..= Aeson.object ["token1" Aeson..= (100 :: Int)]
              ]
          utxo =
            Utxo
              { utxoTxHash = "txhash456"
              , utxoOutputIndex = 1
              , utxoHeadId = "head-abc"
              , utxoAddress = "addr1qxtest"
              , utxoLovelace = 2_000_000
              , utxoAssets = assets
              , utxoDatumHash = Just "datumhash123"
              , utxoInlineDatum = Nothing
              , utxoReferenceScriptHash = Nothing
              , utxoUpdatedAt = now
              }
      let resp = utxoToResponse utxo.utxoHeadId utxo
      length resp.amount `shouldBe` 2
      resp.data_hash `shouldBe` Just "datumhash123"
      let tokenAmounts = filter (\a -> a.unit /= "lovelace") resp.amount
      length tokenAmounts `shouldBe` 1
      (head tokenAmounts).unit `shouldBe` "policy1token1"
      (head tokenAmounts).quantity `shouldBe` "100"

-- | Helper for JSON roundtrip testing
roundtrip :: (ToJSON a, FromJSON a, Eq a) => a -> Maybe a
roundtrip = decode . encode
