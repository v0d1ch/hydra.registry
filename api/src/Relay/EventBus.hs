module Relay.EventBus
  ( EventBus
  , RouteEvent (..)
  , routeEventRouteId
  , routeEventTag
  , newEventBus
  , publish
  , subscribe
  )
where

import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, writeTChan)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | A relay-payment event worth surfacing to a subscribed UI.
--
-- Every constructor carries the @routeId@ as its discriminator so a
-- per-route SSE stream can filter without consulting the DB. The JSON
-- encoding leans on Aeson's default @{ "tag": ..., ...fields... }@
-- shape so the frontend can switch on @data.tag@.
data RouteEvent
  = -- | A new HTLC UTxO was observed at the script address with a
    --   datum that matched a pending hop. The hop is now @locked@ and
    --   the upstream side can proceed once this lock confirms.
    HopLocked
      { routeId :: Text
      , hopIndex :: Int
      , txHash :: Text
      , at :: UTCTime
      }
  | -- | A previously locked hop's HTLC UTxO disappeared from the
    --   snapshot (it was spent, i.e. claimed). The downstream side
    --   has revealed the preimage; upstream bridges can now claim
    --   their own locks.
    HopClaimed
      { routeId :: Text
      , hopIndex :: Int
      , at :: UTCTime
      }
  | -- | A locked hop's HTLC was spent after its timeout — the locker
    --   took the refund path. Observed by the L1 scan after a head
    --   closed with the hop in flight.
    HopRefunded
      { routeId :: Text
      , hopIndex :: Int
      , at :: UTCTime
      }
  | -- | The receiver (or some downstream party) submitted the
    --   preimage to @POST /relay/preimage/{hash}@. Every upstream
    --   bridge sharing that payment hash can now build a Claim.
    PreimageRevealed
      { routeId :: Text
      , paymentHash :: Text
      }
  | -- | All hops in a route reached @claimed@. The route is settled.
    RouteCompleted
      { routeId :: Text
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Shorthand: extract the routeId for filtering.
routeEventRouteId :: RouteEvent -> Text
routeEventRouteId = \case
  HopLocked{routeId} -> routeId
  HopClaimed{routeId} -> routeId
  HopRefunded{routeId} -> routeId
  PreimageRevealed{routeId} -> routeId
  RouteCompleted{routeId} -> routeId

-- | Constructor name as a short tag — used as the SSE
-- @event:@ field name so a frontend can dispatch on it without
-- parsing the JSON body.
routeEventTag :: RouteEvent -> Text
routeEventTag = \case
  HopLocked{} -> "HopLocked"
  HopClaimed{} -> "HopClaimed"
  HopRefunded{} -> "HopRefunded"
  PreimageRevealed{} -> "PreimageRevealed"
  RouteCompleted{} -> "RouteCompleted"

-- | Fan-out broadcast channel. Every 'subscribe' returns an
-- independent reader; events 'publish'ed after a subscriber attaches
-- are visible to that subscriber. Events published before any
-- subscriber attached are dropped — there is no replay.
newtype EventBus = EventBus
  { busChan :: TChan RouteEvent
  }

newEventBus :: IO EventBus
newEventBus = EventBus <$> newBroadcastTChanIO

-- | Send an event to every current subscriber. Cheap; no IO beyond
-- the STM transaction.
publish :: EventBus -> RouteEvent -> IO ()
publish bus ev = atomically (writeTChan bus.busChan ev)

-- | Get a fresh per-subscriber reader. The returned 'TChan' only
-- yields events 'publish'ed after this call; older events are gone.
-- Reading from the returned channel does not drain other
-- subscribers' channels.
subscribe :: EventBus -> IO (TChan RouteEvent)
subscribe bus = atomically (dupTChan bus.busChan)
