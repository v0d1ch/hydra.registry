module Metrics where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Network.Wai

data Metrics = Metrics
  { requestCount :: TVar (Map Text Int)
  , headStatuses :: TVar (Map Text Text)
  }

newMetrics :: IO Metrics
newMetrics = do
  rc <- newTVarIO Map.empty
  hs <- newTVarIO Map.empty
  pure Metrics{requestCount = rc, headStatuses = hs}

incRequestCount :: Metrics -> Text -> IO ()
incRequestCount m key =
  atomically $ modifyTVar' m.requestCount $ Map.insertWith (+) key 1

updateHeadMetric :: Metrics -> Text -> Text -> IO ()
updateHeadMetric m hid status =
  atomically $ modifyTVar' m.headStatuses $ Map.insert hid status

removeHeadMetric :: Metrics -> Text -> IO ()
removeHeadMetric m hid =
  atomically $ modifyTVar' m.headStatuses $ Map.delete hid

-- | Render metrics in Prometheus text format
renderMetrics :: Metrics -> IO Text
renderMetrics m = do
  rc <- readTVarIO m.requestCount
  hs <- readTVarIO m.headStatuses
  pure $
    T.unlines $
      [ "# HELP http_requests_total Total HTTP requests"
      , "# TYPE http_requests_total counter"
      ]
        <> [ T.concat ["http_requests_total{path=\"", k, "\"} ", T.pack (show v)]
           | (k, v) <- Map.toList rc
           ]
        <> [ "# HELP hydra_head_status Status of connected Hydra heads"
           , "# TYPE hydra_head_status gauge"
           ]
        <> [ T.concat ["hydra_head_status{head_id=\"", k, "\",status=\"", v, "\"} 1"]
           | (k, v) <- Map.toList hs
           ]

-- | WAI middleware that counts requests per path
metricsMiddleware :: Metrics -> Middleware
metricsMiddleware m app req respond = do
  let path = "/" <> T.intercalate "/" (pathInfo req)
  incRequestCount m path
  app req respond
