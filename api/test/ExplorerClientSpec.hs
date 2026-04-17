module ExplorerClientSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=))
import Data.Aeson qualified as Aeson
import Explorer.Client (ExplorerHeadEntry (..))
import Test.Hspec

spec :: Spec
spec = describe "Explorer.Client" $ do
  describe "ExplorerHeadEntry JSON parsing" $ do
    it "parses a minimal entry" $ do
      let json =
            object
              [ "headId" .= ("abc123" :: String)
              , "network" .= ("Mainnet" :: String)
              , "networkMagic" .= (764824073 :: Int)
              , "version" .= ("0.21.0" :: String)
              , "status" .= ("Open" :: String)
              ]
      let result = Aeson.decode @ExplorerHeadEntry (encode json)
      case result of
        Nothing -> expectationFailure "Failed to parse minimal entry"
        Just entry -> do
          entry.headId `shouldBe` "abc123"
          entry.network `shouldBe` "Mainnet"
          entry.networkMagic `shouldBe` 764824073
          entry.version `shouldBe` "0.21.0"
          entry.status `shouldBe` "Open"
          entry.contestationPeriod `shouldBe` Nothing
          entry.members `shouldBe` Nothing

    it "parses an entry with all optional fields" $ do
      let membersJson =
            Aeson.toJSON
              [ object
                  [ "party" .= object ["vkey" .= ("aabb" :: String)]
                  , "onChainId" .= ("ccdd" :: String)
                  ]
              ]
          json =
            object
              [ "headId" .= ("def456" :: String)
              , "network" .= ("Testnet" :: String)
              , "networkMagic" .= (1 :: Int)
              , "version" .= ("0.20.0" :: String)
              , "status" .= ("Closed" :: String)
              , "contestationPeriod" .= (60 :: Int)
              , "contestations" .= (2 :: Int)
              , "snapshotNumber" .= (15 :: Int)
              , "contestationDeadline" .= ("2025-01-01T00:00:00Z" :: String)
              , "blockNo" .= (99999 :: Int)
              , "members" .= membersJson
              , "seedTxIn" .= ("txid#0" :: String)
              , "point" .= object ["tag" .= ("BlockPoint" :: String), "slot" .= (12345 :: Int)]
              ]
      let result = Aeson.decode @ExplorerHeadEntry (encode json)
      case result of
        Nothing -> expectationFailure "Failed to parse full entry"
        Just entry -> do
          entry.headId `shouldBe` "def456"
          entry.status `shouldBe` "Closed"
          entry.contestationPeriod `shouldBe` Just 60
          entry.contestations `shouldBe` Just 2
          entry.snapshotNumber `shouldBe` Just 15
          entry.seedTxIn `shouldBe` Just "txid#0"
          entry.blockNo `shouldBe` Just 99999

    it "roundtrips through JSON" $ do
      let entry =
            ExplorerHeadEntry
              { headId = "head123"
              , network = "Mainnet"
              , networkMagic = 764824073
              , version = "0.21.0"
              , status = "Open"
              , contestationPeriod = Just 120
              , contestations = Nothing
              , snapshotNumber = Just 3
              , contestationDeadline = Nothing
              , point = Nothing
              , blockNo = Just 5000
              , members = Nothing
              , seedTxIn = Just "seed#0"
              }
      (Aeson.decode @ExplorerHeadEntry (encode entry)) `shouldBe` Just entry

    it "parses a list of entries (like /heads response)" $ do
      let json =
            Aeson.toJSON
              [ object
                  [ "headId" .= ("h1" :: String)
                  , "network" .= ("Mainnet" :: String)
                  , "networkMagic" .= (764824073 :: Int)
                  , "version" .= ("0.21.0" :: String)
                  , "status" .= ("Open" :: String)
                  ]
              , object
                  [ "headId" .= ("h2" :: String)
                  , "network" .= ("Testnet" :: String)
                  , "networkMagic" .= (1 :: Int)
                  , "version" .= ("0.20.0" :: String)
                  , "status" .= ("Finalized" :: String)
                  ]
              ]
      let result = Aeson.decode @[ExplorerHeadEntry] (encode json)
      case result of
        Nothing -> expectationFailure "Failed to parse list"
        Just entries -> do
          length entries `shouldBe` 2
          (head entries).headId `shouldBe` "h1"
          (entries !! 1).status `shouldBe` "Finalized"
