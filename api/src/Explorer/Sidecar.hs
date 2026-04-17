module Explorer.Sidecar where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (toJSON)
import Data.Aeson qualified as Aeson
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as T
import Db qualified
import Db.Schema (Head (..))
import Explorer.Client (ExplorerHeadEntry (..))
import Hasql.Pool (Pool)
import Logging (Logger, logError, logInfo, logWarn)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP

-- | Configuration for the explorer sidecar
data SidecarConfig = SidecarConfig
  { explorerUrl :: Text
  , pollIntervalSeconds :: Int
  }
  deriving stock (Show, Eq)

-- | Start the explorer sidecar polling loop.
-- This function blocks forever — run it in a separate thread.
startSidecar :: Logger -> Pool -> SidecarConfig -> IO ()
startSidecar logger pool config = do
  manager <- HTTP.newTlsManager
  logInfo logger "Explorer sidecar started" [("url", toJSON config.explorerUrl), ("interval_s", toJSON config.pollIntervalSeconds)]
  forever $ do
    result <- try @SomeException $ pollExplorer logger pool manager config.explorerUrl
    case result of
      Left err ->
        logError logger "Explorer sidecar poll failed" [("error", toJSON (show err))]
      Right () -> pure ()
    threadDelay (config.pollIntervalSeconds * 1_000_000)

-- | Poll the hydra-explorer /heads endpoint and sync to DB
pollExplorer :: Logger -> Pool -> HTTP.Manager -> Text -> IO ()
pollExplorer logger pool manager baseUrl = do
  let url = T.unpack baseUrl <> "/heads"
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager
  let body = HTTP.responseBody response
  case Aeson.eitherDecode body of
    Left err -> do
      logWarn logger "Failed to parse explorer response" [("error", toJSON err)]
    Right (entries :: [ExplorerHeadEntry]) -> do
      logInfo logger "Explorer poll complete" [("heads_found", toJSON (length entries))]
      mapM_ (syncExplorerHead logger pool) entries
      reconcileStatuses logger pool entries

-- | Sync a single explorer head entry to the DB
syncExplorerHead :: Logger -> Pool -> ExplorerHeadEntry -> IO ()
syncExplorerHead _logger pool entry = do
  Db.upsertExplorerHead
    pool
    entry.headId
    entry.network
    entry.networkMagic
    entry.version
    entry.status
    entry.contestationPeriod
    entry.contestations
    entry.snapshotNumber
    entry.contestationDeadline
    entry.point
    entry.blockNo
    entry.members
    entry.seedTxIn

-- | Reconcile: if explorer says Closed/Finalized for a registered head, update it
reconcileStatuses :: Logger -> Pool -> [ExplorerHeadEntry] -> IO ()
reconcileStatuses logger pool entries = do
  let terminalEntries = filter (\e -> e.status `elem` ["Closed", "Finalized"]) entries
  mapM_ reconcileOne terminalEntries
 where
  reconcileOne :: ExplorerHeadEntry -> IO ()
  reconcileOne entry = do
    mHead <- Db.getHead pool entry.headId
    case mHead of
      Nothing -> pure () -- not registered, nothing to reconcile
      Just (h :: Head Identity)
        | h.headStatus /= entry.status -> do
            logInfo
              logger
              "Reconciling head status from explorer"
              [ ("headId", toJSON entry.headId)
              , ("old_status", toJSON h.headStatus)
              , ("new_status", toJSON entry.status)
              ]
            Db.updateHeadStatus pool entry.headId entry.status
        | otherwise -> pure ()
