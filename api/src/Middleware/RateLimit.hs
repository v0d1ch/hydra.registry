module Middleware.RateLimit where

import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Types.Status (status429)
import Network.Wai

data RateLimiter = RateLimiter
  { limitPerMin :: Int
  , clients :: TVar (Map Text [UTCTime])
  }

newRateLimiter :: Int -> IO RateLimiter
newRateLimiter limit = do
  cs <- newTVarIO Map.empty
  pure RateLimiter{limitPerMin = limit, clients = cs}

-- | Check if a request is allowed under the rate limit.
-- Returns True if allowed, False if rate limited.
checkRateLimit :: RateLimiter -> Text -> IO Bool
checkRateLimit limiter clientIp = do
  now <- getCurrentTime
  let windowStart = addUTCTime (-60) now
  atomically $ do
    m <- readTVar limiter.clients
    let hits = filter (> windowStart) $ Map.findWithDefault [] clientIp m
    if length hits >= limiter.limitPerMin
      then pure False
      else do
        writeTVar limiter.clients $ Map.insert clientIp (now : hits) m
        pure True

rateLimitMiddleware :: RateLimiter -> Middleware
rateLimitMiddleware limiter app req respond = do
  let clientIp = getClientIp req
  allowed <- checkRateLimit limiter clientIp
  if allowed
    then app req respond
    else
      respond $
        responseLBS
          status429
          [("Content-Type", "application/json")]
          "{\"error\":\"Rate limit exceeded\"}"

getClientIp :: Request -> Text
getClientIp req =
  case lookup "X-Forwarded-For" (requestHeaders req) of
    Just xff -> decodeUtf8 $ BS.takeWhile (/= 44) xff
    Nothing -> T.pack $ show $ remoteHost req

-- | Remove expired entries from the rate limiter
cleanupRateLimiter :: RateLimiter -> IO ()
cleanupRateLimiter limiter = do
  now <- getCurrentTime
  let windowStart = addUTCTime (-60) now
  atomically $ modifyTVar' limiter.clients $ Map.map (filter (> windowStart))
