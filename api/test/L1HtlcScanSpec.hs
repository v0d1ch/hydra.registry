module L1HtlcScanSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Db qualified
import Db.Schema (RouteHop (..))
import Hasql.Pool (Pool)
import L1.HtlcScan
import Logging (newLogger)
import Relay.HtlcWatcher (Settlement (..), classifySettlement)
import Logging qualified
import Relay.EventBus qualified as Bus
import Test.Hspec
import TestUtils

secretHash :: Text
secretHash = "aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11aa11"

senderPkh, bridgePkh, receiverPkh :: Text
senderPkh = "000000000000000000000000000000000000000000000000000000a1"
bridgePkh = "000000000000000000000000000000000000000000000000000000a2"
receiverPkh = "000000000000000000000000000000000000000000000000000000a3"

spec :: Spec
spec = describe "L1 HTLC scan" $ do
  describe "classifySettlement (pure)" $ do
    -- Spend happened in (lastSeen, tip]. Claim requires validity upper
    -- strictly before timeout; refund requires validity lower strictly
    -- after timeout — the windows are disjoint around the timeout.
    it "classifies as claimed when the whole window is before the timeout" $
      classifySettlement 800 500 1000 `shouldBe` SettledClaimed

    it "classifies as refunded when last seen at or after the timeout" $
      classifySettlement 1600 1000 1000 `shouldBe` SettledRefunded

    it "is ambiguous when the window straddles the timeout" $
      classifySettlement 1100 900 1000 `shouldBe` SettlementAmbiguous

    it "boundary: tip exactly one before timeout is still a claim" $
      classifySettlement 999 500 1000 `shouldBe` SettledClaimed

    it "boundary: tip equal to timeout is ambiguous, not a claim" $
      classifySettlement 1000 900 1000 `shouldBe` SettlementAmbiguous

  describe "applyHtlcScan (integration)" $ around withTestPool $ do
    it "records the tip slot for a locked hop observed at the script address" $ \pool -> do
      seedRoute pool
      bus <- Bus.newEventBus
      let logger = newLogger Logging.Error
      applyHtlcScan logger pool bus 500 [HtlcObservation secretHash senderPkh bridgePkh]
      hop <- hopByIndex pool 0
      hop.hopHtlcLastSeenSlot `shouldBe` Just 500
      hop.hopHtlcStatus `shouldBe` "locked"

    it "marks the hop claimed when it disappears before the timeout" $ \pool -> do
      seedRoute pool
      bus <- Bus.newEventBus
      let logger = newLogger Logging.Error
      applyHtlcScan logger pool bus 500 [HtlcObservation secretHash senderPkh bridgePkh]
      applyHtlcScan logger pool bus 800 []
      hop <- hopByIndex pool 0
      hop.hopHtlcStatus `shouldBe` "claimed"

    it "marks the hop refunded when it disappears after the timeout" $ \pool -> do
      seedRoute pool
      bus <- Bus.newEventBus
      let logger = newLogger Logging.Error
      applyHtlcScan logger pool bus 1500 [HtlcObservation secretHash senderPkh bridgePkh]
      applyHtlcScan logger pool bus 1600 []
      hop <- hopByIndex pool 0
      hop.hopHtlcStatus `shouldBe` "refunded"

    it "leaves the hop locked when the disappearance window straddles the timeout" $ \pool -> do
      seedRoute pool
      bus <- Bus.newEventBus
      let logger = newLogger Logging.Error
      applyHtlcScan logger pool bus 900 [HtlcObservation secretHash senderPkh bridgePkh]
      applyHtlcScan logger pool bus 1100 []
      hop <- hopByIndex pool 0
      hop.hopHtlcStatus `shouldBe` "locked"

    it "matches hops by receiver, not just secret hash" $ \pool -> do
      seedRoute pool
      bus <- Bus.newEventBus
      let logger = newLogger Logging.Error
      -- Observation matches hop 1's receiver — hop 0 must be untouched.
      applyHtlcScan logger pool bus 500 [HtlcObservation secretHash bridgePkh receiverPkh]
      h0 <- hopByIndex pool 0
      h1 <- hopByIndex pool 1
      h0.hopHtlcLastSeenSlot `shouldBe` Nothing
      h1.hopHtlcLastSeenSlot `shouldBe` Just 500

    it "never settles a hop that was never observed on L1" $ \pool -> do
      seedRoute pool
      bus <- Bus.newEventBus
      let logger = newLogger Logging.Error
      applyHtlcScan logger pool bus 800 []
      hop <- hopByIndex pool 0
      hop.hopHtlcStatus `shouldBe` "locked"

-- | Two locked hops sharing the secret hash (as real cascades do),
-- distinguished by receiver. Timeout slot 1000 on both.
seedRoute :: Pool -> IO ()
seedRoute pool = do
  now <- getCurrentTime
  Db.insertPaymentRoute pool "route-l1" "inv-l1" senderPkh receiverPkh 5_000_000 "in_progress" (Aeson.toJSON ([] :: [Value])) 0 "Preprod"
  Db.insertRouteHops
    pool
    [ ("hop-l1-0", "route-l1", 0, "head-a", bridgePkh, senderPkh, bridgePkh, "pending", secretHash, 1000, 0)
    , ("hop-l1-1", "route-l1", 1, "head-b", bridgePkh, bridgePkh, receiverPkh, "pending", secretHash, 1000, 0)
    ]
  Db.updateHopLocked pool "hop-l1-0" "lock-tx-0" now
  Db.updateHopLocked pool "hop-l1-1" "lock-tx-1" now

hopByIndex :: Pool -> Int -> IO (RouteHop Identity)
hopByIndex pool i = do
  hops <- Db.getRouteHops pool "route-l1"
  case [h | h <- hops, fromIntegral h.hopIndex == i] of
    [h] -> pure h
    _ -> fail $ "expected hop " <> show i
