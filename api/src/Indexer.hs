module Indexer where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (toJSON)
import Data.Text (Text)
import Db qualified
import Db.Schema qualified as Schema
import Hasql.Pool (Pool)
import Hydra.Client
import Logging

-- | Run the indexer loop that processes events from all Hydra head connections.
-- This function blocks forever — run it in a separate thread.
startIndexer :: Logger -> Pool -> TQueue HydraEvent -> IO ()
startIndexer logger pool eventQueue = forever $ do
  event <- atomically $ readTQueue eventQueue
  result <- try @SomeException $ processEvent logger pool event
  case result of
    Left err ->
      logError logger "Error processing indexer event" [("error", toJSON (show err))]
    Right () -> pure ()

-- | Process a single Hydra event
processEvent :: Logger -> Pool -> HydraEvent -> IO ()
processEvent logger pool = \case
  HeadGreetings{greeterHeadId, greeterHeadStatus, greeterUtxos} -> do
    logInfo logger "Head greeting received" [("headId", toJSON greeterHeadId), ("status", toJSON greeterHeadStatus)]
    Db.updateHeadStatus pool greeterHeadId greeterHeadStatus
    Db.replaceUtxos pool greeterHeadId greeterUtxos
    Db.updateLastMessageAt pool greeterHeadId
  HeadSnapshotConfirmed{snapHeadId, snapNumber, snapUtxos} -> do
    logInfo logger "Snapshot confirmed" [("headId", toJSON snapHeadId), ("snapshot", toJSON snapNumber), ("utxoCount", toJSON (length snapUtxos))]
    Db.updateHeadStatus pool snapHeadId "Open"
    Db.updateSnapshotNumber pool snapHeadId snapNumber
    Db.replaceUtxos pool snapHeadId snapUtxos
    Db.updateLastMessageAt pool snapHeadId
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
