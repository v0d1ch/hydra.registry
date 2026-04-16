module Cache where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

data Cache a = Cache
  { store :: TVar (Map Text (a, UTCTime))
  , ttlSeconds :: NominalDiffTime
  }

newCache :: NominalDiffTime -> IO (Cache a)
newCache ttl = do
  s <- newTVarIO Map.empty
  pure Cache{store = s, ttlSeconds = ttl}

lookupCache :: Cache a -> Text -> IO (Maybe a)
lookupCache cache key = do
  now <- getCurrentTime
  atomically $ do
    m <- readTVar cache.store
    case Map.lookup key m of
      Just (val, ts) | addUTCTime cache.ttlSeconds ts > now -> pure (Just val)
      Just _ -> do
        modifyTVar' cache.store (Map.delete key)
        pure Nothing
      Nothing -> pure Nothing

insertCache :: Cache a -> Text -> a -> IO ()
insertCache cache key val = do
  now <- getCurrentTime
  atomically $ modifyTVar' cache.store (Map.insert key (val, now))

invalidateCache :: Cache a -> Text -> IO ()
invalidateCache cache key =
  atomically $ modifyTVar' cache.store (Map.delete key)

invalidateAll :: Cache a -> IO ()
invalidateAll cache =
  atomically $ writeTVar cache.store Map.empty
