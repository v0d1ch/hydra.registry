module Relay.ExpirySweep
  ( startExpirySweep
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (toJSON)
import Data.Time (getCurrentTime)
import Db qualified
import Hasql.Pool (Pool)
import Logging

-- | Periodically expire stale invoices and routes.
-- Runs every 60 seconds. Marks pending invoices past their deadline as expired,
-- then cascades to routes that reference those invoices.
startExpirySweep :: Logger -> Pool -> IO ()
startExpirySweep logger pool = forever $ do
  threadDelay 60_000_000 -- 60 seconds
  result <- try @SomeException $ do
    now <- getCurrentTime
    Db.expirePendingInvoices pool now
    Db.expireStaleRoutes pool now
  case result of
    Left err ->
      logError logger "Expiry sweep failed" [("error", toJSON (show err))]
    Right () -> pure ()
