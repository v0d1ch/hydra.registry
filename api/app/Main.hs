module Main where

import Api (AppEnv (..), api, corsMiddleware, server)
import Blockfrost qualified
import Cache (newCache)
import Config (AppConfig (..), loadConfig)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Db qualified
import Explorer.Sidecar qualified as Sidecar
import Hydra.Client (HydraEvent)
import Relay.EventBus qualified as EventBus
import Relay.ExpirySweep qualified as ExpirySweep
import Relay.Graph qualified as Graph
import Indexer qualified
import Logging
import Metrics (metricsMiddleware, newMetrics)
import Middleware.RateLimit (cleanupRateLimiter, newRateLimiter, rateLimitMiddleware)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant (serve)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)

main :: IO ()
main = do
  config <- loadConfig
  let logger = newLogger Info

  logInfo logger "Starting Hydra Registry API..." []

  -- Initialize database
  pool <- Db.createPool config.dbConnStr
  Db.initDb pool
  logInfo logger "Database initialized" []

  -- Create event queue
  eventQueue <- newTQueueIO @HydraEvent

  -- Track L1 chain tip seen via Greetings; used to derive route timeouts
  -- without trusting the registry's local system clock.
  chainSlotVar <- newTVarIO (0 :: Int64)

  -- Relay event bus — fan-out for HTLC lock/claim/preimage transitions
  -- so SSE subscribers get pushed deltas instead of having to poll.
  bus <- EventBus.newEventBus

  -- Start the indexer (runs forever in its own thread)
  indexerAsync <- async $ Indexer.startIndexer logger pool chainSlotVar bus config.htlcScriptHash eventQueue
  logInfo logger "Indexer started" []

  -- Reconnect to registered heads — only in direct-WS (dev) mode; in
  -- production, events arrive exclusively via agent push.
  if config.directWs
    then do
      Indexer.reconnectAllHeads logger pool eventQueue
      logInfo logger "Reconnected to registered heads" []
    else
      logInfo logger "Direct WS disabled — relying on agent push for head events" []

  -- Initialize relay graph
  relayGraphVar <- newTVarIO Graph.emptyGraph

  -- Start explorer sidecar (polls hydra-explorer every N seconds)
  let sidecarConfig =
        Sidecar.SidecarConfig
          { explorerUrl = config.explorerUrl
          , pollIntervalSeconds = config.explorerPollIntervalSeconds
          , relayGraphVar = relayGraphVar
          , defaultNetwork = config.defaultNetwork
          , l1Sockets = config.l1Sockets
          }
  sidecarAsync <- async $ Sidecar.startSidecar logger pool sidecarConfig
  logInfo logger "Explorer sidecar started" [("url", toJSON config.explorerUrl), ("interval_s", toJSON config.explorerPollIntervalSeconds)]

  -- Start expiry sweep (marks stale invoices/routes as expired every 60s)
  expiryAsync <- async $ ExpirySweep.startExpirySweep logger pool
  logInfo logger "Expiry sweep started" []

  -- Optionally poll Blockfrost for chain slot (runs alongside Hydra events)
  mBlockfrostAsync <- case config.blockfrostProjectId of
    Nothing -> do
      logInfo logger "Blockfrost not configured (set BLOCKFROST_PROJECT_ID to enable)" []
      pure Nothing
    Just pid -> do
      a <- async $ Blockfrost.startBlockfrostPoller logger chainSlotVar pid config.blockfrostNetwork
      pure (Just a)

  -- Rate limiter with periodic cleanup
  rateLimiter <- newRateLimiter config.rateLimitPerMin
  cleanupAsync <- async $ forever $ do
    threadDelay 60_000_000
    cleanupRateLimiter rateLimiter

  -- Metrics
  metrics <- newMetrics

  -- Address cache (30 second TTL)
  addrCache <- newCache 30

  -- Graceful shutdown
  shutdownVar <- newEmptyMVar
  let shutdown = putMVar shutdownVar ()
  void $ installHandler sigTERM (Catch shutdown) Nothing
  void $ installHandler sigINT (Catch shutdown) Nothing

  -- Application environment
  let env =
        AppEnv
          { pool = pool
          , eventQueue = eventQueue
          , logger = logger
          , metrics = metrics
          , addressCache = addrCache
          , staticDir = config.staticDir
          , relayGraph = relayGraphVar
          , latestChainSlot = chainSlotVar
          , relayEventBus = bus
          , htlcScriptHash = config.htlcScriptHash
          , htlcScriptCbor = config.htlcScriptCbor
          , cardanoNodeSocket = config.cardanoNodeSocket
          , cardanoNodeMagic = config.cardanoNodeMagic
          , directWs = config.directWs
          }

  -- Build middleware stack
  let middleware =
        logStdout
          . corsMiddleware
          . rateLimitMiddleware rateLimiter
          . metricsMiddleware metrics

  -- Start HTTP server
  let app = middleware $ serve api (server env)
      settings =
        Warp.setPort config.httpPort $
          Warp.setGracefulShutdownTimeout (Just 5) $
            Warp.setInstallShutdownHandler
              (\closeSocket -> void $ forkIO $ takeMVar shutdownVar >> closeSocket)
              Warp.defaultSettings

  logInfo logger "Listening" [("port", toJSON config.httpPort)]
  Warp.runSettings settings app
    `finally` do
      logInfo logger "Shutting down..." []
      cancel indexerAsync
      cancel sidecarAsync
      cancel expiryAsync
      cancel cleanupAsync
      mapM_ cancel mBlockfrostAsync
      logInfo logger "Shutdown complete" []

