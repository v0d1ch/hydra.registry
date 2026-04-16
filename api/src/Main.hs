module Main where

import Api (api, server)
import Control.Concurrent.STM
import Db qualified
import Hydra.Client (HydraEvent)
import Indexer qualified
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (serve)
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting Hydra Registry API..."

  -- Initialize database
  let connStr = "host=/tmp port=5432 dbname=hydra_registry"
  pool <- Db.createPool connStr
  Db.initDb pool
  putStrLn "Database initialized."

  -- Create event queue for Hydra head connections
  eventQueue <- newTQueueIO @HydraEvent

  -- Start the indexer
  Indexer.startIndexer pool eventQueue
  putStrLn "Indexer started."

  -- Reconnect to all previously registered heads
  Indexer.reconnectAllHeads pool eventQueue
  putStrLn "Reconnected to registered heads."

  -- Start the HTTP server
  let port = 8080
  putStrLn $ "Listening on port " <> show port
  Warp.run port $ simpleCors $ serve api (server pool eventQueue)
