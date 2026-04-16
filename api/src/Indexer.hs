module Indexer where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.Text (Text)
import Db qualified
import Db.Schema qualified as Schema
import Hasql.Pool (Pool)
import Hydra.Client

-- | Start the indexer that processes events from all Hydra head connections
startIndexer :: Pool -> TQueue HydraEvent -> IO ()
startIndexer pool eventQueue = void $ forkIO $ forever $ do
  event <- atomically $ readTQueue eventQueue
  processEvent pool event

-- | Process a single Hydra event
processEvent :: Pool -> HydraEvent -> IO ()
processEvent pool = \case
  HeadGreetings{greeterHeadId, greeterHeadStatus, greeterUtxos} -> do
    Db.updateHeadStatus pool greeterHeadId greeterHeadStatus
    Db.replaceUtxos pool greeterHeadId greeterUtxos
  HeadSnapshotConfirmed{snapHeadId, snapUtxos} -> do
    Db.updateHeadStatus pool snapHeadId "Open"
    Db.replaceUtxos pool snapHeadId snapUtxos
  HeadClosed{closedHeadId} -> do
    Db.updateHeadStatus pool closedHeadId "Closed"
  HeadFinalized{finalizedHeadId, finalizedUtxos} -> do
    Db.updateHeadStatus pool finalizedHeadId "Finalized"
    Db.replaceUtxos pool finalizedHeadId finalizedUtxos
  ConnectionLost ->
    pure ()

-- | Register a new head: validate, store in DB, start listening
registerHead :: Pool -> TQueue HydraEvent -> Text -> Int -> IO (Either Text HydraEvent)
registerHead pool eventQueue hostAddr portNum = do
  result <- validateHydraNode hostAddr portNum
  case result of
    Left err -> pure $ Left err
    Right evt@HeadGreetings{greeterHeadId, greeterHeadStatus, greeterUtxos} -> do
      Db.upsertHead pool greeterHeadId hostAddr portNum greeterHeadStatus
      Db.replaceUtxos pool greeterHeadId greeterUtxos
      connectToHead hostAddr portNum eventQueue
      pure $ Right evt
    Right _ -> pure $ Left "Unexpected event during validation"

-- | Reconnect to all registered heads on startup
reconnectAllHeads :: Pool -> TQueue HydraEvent -> IO ()
reconnectAllHeads pool eventQueue = do
  heads <- Db.getAllHeads pool
  mapM_ reconnect heads
 where
  reconnect h =
    connectToHead h.headHost (fromIntegral h.headPort) eventQueue
