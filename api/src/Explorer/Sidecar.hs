module Explorer.Sidecar where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (toJSON)
import Data.Aeson qualified as Aeson
import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Db qualified
import Db.Schema (ExplorerHead (..), Head (..), HeadParticipant (..))
import Explorer.Client (ExplorerHeadEntry (..))
import Explorer.Members (parseMembers, participantAddress, participantToTuple)
import Hasql.Pool (Pool)
import Logging (Logger, logError, logInfo, logWarn)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Relay.Graph qualified as Graph

-- | Configuration for the explorer sidecar
data SidecarConfig = SidecarConfig
  { explorerUrl :: Text
  , pollIntervalSeconds :: Int
  , relayGraphVar :: TVar Graph.RelayGraph
  , sidecarHtlcScriptHash :: Maybe Text
  }

-- | Start the explorer sidecar polling loop.
-- This function blocks forever — run it in a separate thread.
startSidecar :: Logger -> Pool -> SidecarConfig -> IO ()
startSidecar logger pool config = do
  manager <- HTTP.newTlsManager
  logInfo logger "Explorer sidecar started" [("url", toJSON config.explorerUrl), ("interval_s", toJSON config.pollIntervalSeconds)]
  forever $ do
    result <- try @SomeException $ pollExplorer logger pool manager config
    case result of
      Left err ->
        logError logger "Explorer sidecar poll failed" [("error", toJSON (show err))]
      Right () -> pure ()
    threadDelay (config.pollIntervalSeconds * 1_000_000)

-- | Poll the hydra-explorer /heads endpoint and sync to DB
pollExplorer :: Logger -> Pool -> HTTP.Manager -> SidecarConfig -> IO ()
pollExplorer logger pool manager config = do
  let url = T.unpack config.explorerUrl <> "/heads"
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager
  let body = HTTP.responseBody response
  case Aeson.eitherDecode body of
    Left err -> do
      logWarn logger "Failed to parse explorer response" [("error", toJSON err)]
    Right (entries :: [ExplorerHeadEntry]) -> do
      logInfo logger "Explorer poll complete" [("heads_found", toJSON (length entries))]
      mapM_ (syncExplorerHead logger pool) entries
      mapM_ (syncParticipants logger pool) entries
      reconcileStatuses logger pool entries
      rebuildRelayGraph logger pool config

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

-- | Sync participants for a head from the members JSON
syncParticipants :: Logger -> Pool -> ExplorerHeadEntry -> IO ()
syncParticipants logger pool entry = do
  let participants = parseMembers entry.members
      tuples = map participantToTuple participants
  case tuples of
    [] -> pure ()
    _ -> do
      Db.replaceHeadParticipants pool entry.headId tuples
      logInfo
        logger
        "Synced participants"
        [ ("headId", toJSON entry.headId)
        , ("count", toJSON (length tuples))
        ]

-- | Rebuild the relay graph from current DB state
rebuildRelayGraph :: Logger -> Pool -> SidecarConfig -> IO ()
rebuildRelayGraph logger pool config = do
  explorerHeads <- Db.getAllExplorerHeads pool
  let getEhId (ExplorerHead{explorerHeadId = hid}) = hid
      getEhNet (ExplorerHead{explorerNetwork = net}) = net
      heads = [(getEhId eh, getEhNet eh) | eh <- explorerHeads]

  -- Get all participants
  let getParticipantAddr (HeadParticipant{participantAddress = addr}) = addr
  allParticipants <- concat <$> mapM (\(hid, _) -> do
    ps <- Db.getParticipantsForHead pool hid
    pure [(hid, getParticipantAddr hp) | hp <- ps]) heads

  -- Get bridge heads
  let getHId (Head{headId = hid}) = hid
      getBridgeFee (Head{headBridgeFeeLovelace = fee}) = fee
  bridgeHeadsDb <- Db.getBridgeHeads pool
  let bridgeHeadIds = Set.fromList [getHId h | h <- bridgeHeadsDb]
      bridgeFees = Map.fromList
        [(getHId h, maybe 0 id (getBridgeFee h)) | h <- bridgeHeadsDb]

  -- Get heads with HTLC script
  htlcHeadIds <- case config.sidecarHtlcScriptHash of
    Nothing -> pure Set.empty
    Just scriptHash -> Set.fromList <$> Db.getHeadsWithScript pool scriptHash

  let graph = Graph.buildGraph heads allParticipants bridgeHeadIds bridgeFees htlcHeadIds
  atomically $ writeTVar config.relayGraphVar graph
  logInfo logger "Relay graph rebuilt"
    [ ("nodes", toJSON (Map.size graph.graphNodes))
    , ("edges", toJSON (length graph.graphEdges))
    ]

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
