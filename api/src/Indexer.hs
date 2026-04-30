module Indexer where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Db qualified
import Db.Schema (Head (..))
import Hasql.Pool (Pool)
import Hydra.Client
import Logging
import Relay.EventBus (EventBus)
import Relay.HtlcWatcher qualified as HtlcWatcher

-- | Run the indexer loop that processes events from all Hydra head connections.
-- This function blocks forever — run it in a separate thread.
--
-- @latestChainSlot@ is bumped on every Greetings/snapshot event that
-- carries a slot — handlers (e.g. @handleFindRoutes@) read it to derive
-- timeouts from chain time rather than the registry's system clock.
startIndexer :: Logger -> Pool -> TVar Int64 -> EventBus -> Maybe Text -> TQueue HydraEvent -> IO ()
startIndexer logger pool chainSlotVar bus mHtlcScriptHash eventQueue = forever $ do
  event <- atomically $ readTQueue eventQueue
  result <- try @SomeException $ processEvent logger pool chainSlotVar bus mHtlcScriptHash event
  case result of
    Left err ->
      logError logger "Error processing indexer event" [("error", toJSON (show err))]
    Right () -> pure ()

-- | Process a single Hydra event
processEvent :: Logger -> Pool -> TVar Int64 -> EventBus -> Maybe Text -> HydraEvent -> IO ()
processEvent logger pool chainSlotVar bus mHtlcScriptHash = \case
  HeadGreetings{greeterHeadId, greeterHeadStatus, greeterUtxos, greeterParticipants, greeterCurrentSlot} -> do
    logInfo logger "Head greeting received" [("headId", toJSON greeterHeadId), ("status", toJSON greeterHeadStatus)]
    bumpChainSlot chainSlotVar greeterCurrentSlot
    Db.updateHeadStatus pool greeterHeadId greeterHeadStatus
    Db.replaceUtxos pool greeterHeadId greeterUtxos
    Db.updateLastMessageAt pool greeterHeadId
    -- Mirror the head's participants into head_participants so the relay
    -- graph picks up shared-participant relationships between
    -- locally-registered heads without depending on the public
    -- hydra-explorer sidecar. The OnChainId hex (pkh of each
    -- participant's @--cardano-signing-key@) goes into the @address@
    -- column; routing matches it against the same form supplied by
    -- senders/receivers when finding a route.
    case greeterParticipants of
      [] -> pure ()
      pids ->
        Db.replaceHeadParticipants pool greeterHeadId
          [(onChainId, Nothing, Just onChainId, 0, Nothing) | onChainId <- pids]
    -- Check for HTLC events
    case mHtlcScriptHash of
      Just scriptHash -> HtlcWatcher.processUtxoSnapshot logger pool bus greeterHeadId scriptHash greeterUtxos
      Nothing -> pure ()
  HeadSnapshotConfirmed{snapHeadId, snapNumber, snapUtxos} -> do
    logInfo logger "Snapshot confirmed" [("headId", toJSON snapHeadId), ("snapshot", toJSON snapNumber), ("utxoCount", toJSON (length snapUtxos))]
    Db.updateHeadStatus pool snapHeadId "Open"
    Db.updateSnapshotNumber pool snapHeadId snapNumber
    Db.replaceUtxos pool snapHeadId snapUtxos
    Db.updateLastMessageAt pool snapHeadId
    -- Check for HTLC events
    case mHtlcScriptHash of
      Just scriptHash -> HtlcWatcher.processUtxoSnapshot logger pool bus snapHeadId scriptHash snapUtxos
      Nothing -> pure ()
  HeadClosed{closedHeadId} -> do
    logInfo logger "Head closed" [("headId", toJSON closedHeadId)]
    Db.updateHeadStatus pool closedHeadId "Closed"
    Db.updateLastMessageAt pool closedHeadId
  HeadFinalized{finalizedHeadId, finalizedUtxos} -> do
    logInfo logger "Head finalized" [("headId", toJSON finalizedHeadId)]
    Db.updateHeadStatus pool finalizedHeadId "Finalized"
    Db.replaceUtxos pool finalizedHeadId finalizedUtxos
    Db.updateLastMessageAt pool finalizedHeadId
  ConnectionLost{lostHeadId} -> do
    logWarn logger "Connection lost to head" [("headId", toJSON lostHeadId)]
    Db.updateHeadStatus pool lostHeadId "unreachable"

-- | Register a new head: validate, store in DB, start listening
registerHead :: Logger -> Pool -> TQueue HydraEvent -> Text -> Int -> IO (Either Text HydraEvent)
registerHead logger pool eventQueue hostAddr portNum = do
  result <- validateHydraNode logger hostAddr portNum
  case result of
    Left err -> pure $ Left err
    Right evt@HeadGreetings{greeterHeadId, greeterHeadStatus, greeterUtxos} -> do
      existing <- Db.getHead pool greeterHeadId
      case existing of
        Just _ -> pure $ Left $ "Head " <> greeterHeadId <> " is already registered"
        Nothing -> do
          Db.upsertHead pool greeterHeadId hostAddr portNum greeterHeadStatus
          Db.replaceUtxos pool greeterHeadId greeterUtxos
          connectToHead logger greeterHeadId hostAddr portNum eventQueue
          logInfo logger "Head registered" [("headId", toJSON greeterHeadId), ("host", toJSON hostAddr)]
          pure $ Right evt
    Right _ -> pure $ Left "Unexpected event during validation"

-- | Reconnect to all registered heads on startup
reconnectAllHeads :: Logger -> Pool -> TQueue HydraEvent -> IO ()
reconnectAllHeads logger pool eventQueue = do
  heads <- Db.getAllHeads pool
  logInfo logger "Reconnecting to registered heads" [("count", toJSON (length heads))]
  mapM_ reconnect heads
 where
  reconnect h =
    connectToHead logger h.headId h.headHost (fromIntegral h.headPort) eventQueue

-- | Update the registry's view of L1 chain time, monotonically.
-- Greetings/snapshot events from any head's WS bring chain ticks; we
-- keep the highest seen so handlers can compute slot-relative deadlines
-- without depending on the registry's local system clock.
bumpChainSlot :: TVar Int64 -> Int64 -> IO ()
bumpChainSlot var newSlot
  | newSlot <= 0 = pure ()
  | otherwise = atomically $ modifyTVar' var (max newSlot)
