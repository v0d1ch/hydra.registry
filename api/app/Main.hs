module Main where

import Api (AppEnv (..), api, corsMiddleware, server)
import Cache (newCache)
import Config (AppConfig (..), loadConfig)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.Aeson (toJSON)
import Db qualified
import Explorer.Sidecar qualified as Sidecar
import Hydra.Client (HydraEvent)
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

  -- Start the indexer (runs forever in its own thread)
  indexerAsync <- async $ Indexer.startIndexer logger pool eventQueue
  logInfo logger "Indexer started" []

  -- Reconnect to registered heads
  Indexer.reconnectAllHeads logger pool eventQueue
  logInfo logger "Reconnected to registered heads" []

  -- Start explorer sidecar (polls hydra-explorer every N seconds)
  let sidecarConfig =
        Sidecar.SidecarConfig
          { explorerUrl = config.explorerUrl
          , pollIntervalSeconds = config.explorerPollIntervalSeconds
          }
  sidecarAsync <- async $ Sidecar.startSidecar logger pool sidecarConfig
  logInfo logger "Explorer sidecar started" [("url", toJSON config.explorerUrl), ("interval_s", toJSON config.explorerPollIntervalSeconds)]

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
      cancel cleanupAsync
      logInfo logger "Shutdown complete" []

