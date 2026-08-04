module RelayEventBusSpec (spec) where

import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Data.Time (getCurrentTime)
import Relay.EventBus
import Test.Hspec

spec :: Spec
spec = describe "Relay.EventBus" $ do
  it "delivers events to a single subscriber" $ do
    bus <- newEventBus
    sub <- subscribe bus
    now <- getCurrentTime
    let ev = HopLocked "route-1" 0 "tx-1" now
    publish bus ev
    delivered <- atomically (readTChan sub)
    delivered `shouldBe` ev

  it "fans out to every subscriber independently" $ do
    bus <- newEventBus
    sub1 <- subscribe bus
    sub2 <- subscribe bus
    sub3 <- subscribe bus
    now <- getCurrentTime
    let ev = HopClaimed "route-1" 0 now
    publish bus ev
    -- Each subscriber's TChan is independent; reading from one
    -- doesn't drain the others.
    a <- atomically (readTChan sub1)
    b <- atomically (readTChan sub2)
    c <- atomically (readTChan sub3)
    a `shouldBe` ev
    b `shouldBe` ev
    c `shouldBe` ev

  it "preserves publish ordering on each subscriber's stream" $ do
    bus <- newEventBus
    sub <- subscribe bus
    now <- getCurrentTime
    let evs =
          [ HopLocked "r" 0 "tx0" now
          , HopLocked "r" 1 "tx1" now
          , PreimageRevealed "r" "ph"
          , HopClaimed "r" 1 now
          , HopClaimed "r" 0 now
          , RouteCompleted "r"
          ]
    mapM_ (publish bus) evs
    delivered <- mapM (\_ -> atomically (readTChan sub)) evs
    delivered `shouldBe` evs

  it "subscribers do not see events published before they subscribed" $ do
    bus <- newEventBus
    now <- getCurrentTime
    -- Published with no subscribers - disappears.
    publish bus (HopLocked "older" 0 "tx-old" now)
    sub <- subscribe bus
    publish bus (HopLocked "newer" 0 "tx-new" now)
    delivered <- atomically (readTChan sub)
    delivered `shouldBe` HopLocked "newer" 0 "tx-new" now
    -- Channel is now empty - the older event was never seen.
    empty <- atomically (isEmptyTChan sub)
    empty `shouldBe` True

  it "draining one subscriber does not affect the others" $ do
    bus <- newEventBus
    sub1 <- subscribe bus
    sub2 <- subscribe bus
    _now <- getCurrentTime
    let ev = RouteCompleted "r"
    publish bus ev
    -- Drain sub1 only.
    _ <- atomically (readTChan sub1)
    -- sub2 still has the event waiting.
    stillPending <- atomically (isEmptyTChan sub2)
    stillPending `shouldBe` False
    delivered <- atomically (readTChan sub2)
    delivered `shouldBe` ev
