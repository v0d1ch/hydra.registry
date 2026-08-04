module Blockfrost (startBlockfrostPoller) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (FromJSON (..), toJSON, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Indexer (bumpChainSlot)
import Logging
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

newtype BlockfrostBlock = BlockfrostBlock {blockSlot :: Int64}

instance FromJSON BlockfrostBlock where
  parseJSON = withObject "BlockfrostBlock" $ \o -> BlockfrostBlock <$> o .: "slot"

baseUrl :: Text -> String
baseUrl network = case network of
  "mainnet" -> "https://cardano-mainnet.blockfrost.io/api/v0"
  "preprod" -> "https://cardano-preprod.blockfrost.io/api/v0"
  _         -> "https://cardano-preview.blockfrost.io/api/v0"

-- | Poll Blockfrost every 60 s and bump the shared chain-slot TVar.
-- Runs forever; launch with 'async'.
startBlockfrostPoller :: Logger -> TVar Int64 -> Text -> Text -> IO ()
startBlockfrostPoller logger chainSlotVar projectId network = do
  manager <- newManager tlsManagerSettings
  logInfo logger "Blockfrost poller started" [("network", toJSON network)]
  forever $ do
    result <- try @SomeException $ do
      req <- parseRequest (baseUrl network <> "/blocks/latest")
      let req' = req{requestHeaders = [("project_id", TE.encodeUtf8 projectId)]}
      resp <- httpLbs req' manager
      case Aeson.decode @BlockfrostBlock (responseBody resp) of
        Just blk -> do
          bumpChainSlot chainSlotVar blk.blockSlot
          logInfo logger "Blockfrost slot bumped" [("slot", toJSON blk.blockSlot)]
        Nothing ->
          logWarn logger "Blockfrost: could not parse block response" []
    case result of
      Left err -> logWarn logger "Blockfrost poll failed" [("error", toJSON (show err))]
      Right () -> pure ()
    threadDelay 60_000_000
