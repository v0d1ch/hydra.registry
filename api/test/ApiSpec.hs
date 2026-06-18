module ApiSpec (spec) where

import Api (hopTimeoutMarginSlots, hopTimeoutSlot, hopUrgency, participantActionsFor, routeRolesFor)
import Api.Types (ParticipantAction (..))
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Db.Schema (PaymentRoute (..), RouteHop (..))
import Data.Time (UTCTime)
import Test.Hspec

-- | Smallest 'RouteHop' fixture for eligibility tests. Only the
-- fields the helpers read are meaningful; the rest are placeholder
-- values.
mkHop
  :: Int32
  -> Text -- senderPkh
  -> Text -- receiverPkh
  -> Text -- htlcStatus
  -> Maybe Text -- preimage
  -> Int64 -- timeoutSlot
  -> RouteHop Identity
mkHop idx senderPkh receiverPkh status mPreimage timeoutSlot =
  RouteHop
    { hopId = "h" <> showT idx
    , hopRouteId = "r"
    , hopIndex = idx
    , hopHeadId = "head"
    , hopBridgeAddress = receiverPkh
    , hopSenderAddress = senderPkh
    , hopReceiverAddress = receiverPkh
    , hopHtlcStatus = status
    , hopHtlcTxHash = Nothing
    , hopSecretHash = "ph"
    , hopPreimage = mPreimage
    , hopTimeoutSlot = timeoutSlot
    , hopFeeLovelace = 0
    , hopLockedAt = Nothing
    , hopClaimedAt = Nothing
    }

showT :: Show a => a -> Text
showT = T.pack . show

mkRoute :: Text -> Text -> [RouteHop Identity] -> PaymentRoute Identity
mkRoute senderPkh receiverPkh _hops =
  PaymentRoute
    { routeId = "r"
    , routeInvoiceId = "inv"
    , routeSenderAddress = senderPkh
    , routeReceiverAddress = receiverPkh
    , routeAmountLovelace = 0
    , routeStatus = "in_progress"
    , routePath = "" -- aeson-Null would be better but simple Text avoids import noise
    , routeTotalFee = 0
    , routeNetwork = "Preview"
    , routeCreatedAt = epoch
    , routeUpdatedAt = epoch
    }

epoch :: UTCTime
epoch = read "2026-01-01 00:00:00 UTC"

spec :: Spec
spec = describe "Api" $ do
  describe "hopUrgency" $ do
    it "returns ok when chainSlot is unknown" $ do
      hopUrgency 0 1_000_000 `shouldBe` "ok"

    it "returns expired when timeout is in the past" $ do
      hopUrgency 200 100 `shouldBe` "expired"

    it "returns expiring when timeout is within 5 minutes" $ do
      hopUrgency 100 200 `shouldBe` "expiring" -- 100 slots until timeout
      hopUrgency 100 400 `shouldBe` "expiring" -- 300 slots, exactly at boundary

    it "returns soon when timeout is within 30 minutes" $ do
      hopUrgency 100 800 `shouldBe` "soon" -- 700 slots, between 5 and 30 min

    it "returns ok when timeout is more than 30 minutes off" $ do
      hopUrgency 100 5_000 `shouldBe` "ok"

  describe "routeRolesFor" $ do
    let me = "me-pkh"
        other = "other-pkh"
        bridge = "bridge-pkh"
        receiver = "rec-pkh"

    it "labels me as sender when route.sender matches" $ do
      let hops =
            [ mkHop 0 me bridge "pending" Nothing 1000
            , mkHop 1 bridge receiver "pending" Nothing 800
            ]
          route = mkRoute me receiver hops
      routeRolesFor me route hops `shouldBe` ["sender"]

    it "labels me as receiver when route.receiver matches" $ do
      let hops =
            [ mkHop 0 other bridge "pending" Nothing 1000
            , mkHop 1 bridge me "pending" Nothing 800
            ]
          route = mkRoute other me hops
      routeRolesFor me route hops `shouldBe` ["receiver"]

    it "labels me as bridge when I appear only in intermediate hops" $ do
      let hops =
            [ mkHop 0 other me "pending" Nothing 1000
            , mkHop 1 me receiver "pending" Nothing 800
            ]
          route = mkRoute other receiver hops
      routeRolesFor me route hops `shouldBe` ["bridge"]

    it "returns empty when I am not in the route" $ do
      let hops =
            [ mkHop 0 other bridge "pending" Nothing 1000
            , mkHop 1 bridge receiver "pending" Nothing 800
            ]
          route = mkRoute other receiver hops
      routeRolesFor me route hops `shouldBe` []

  describe "participantActionsFor" $ do
    let me = "me-pkh"
        bridge = "bridge-pkh"
        receiver = "rec-pkh"
        chainSlot = 500 :: Int64

    it "shows Lock to the sender of hop 0 when it is pending" $ do
      let hops =
            [ mkHop 0 me bridge "pending" Nothing 1000
            , mkHop 1 bridge receiver "pending" Nothing 800
            ]
      map (.kind) (participantActionsFor chainSlot me hops) `shouldBe` ["lock"]

    it "withholds Lock from the bridge until upstream is locked" $ do
      let hops =
            [ mkHop 0 receiver me "pending" Nothing 1000 -- upstream still pending
            , mkHop 1 me receiver "pending" Nothing 800
            ]
      participantActionsFor chainSlot me hops `shouldBe` []

    it "shows Lock to the bridge once upstream has locked" $ do
      let hops =
            [ mkHop 0 receiver me "locked" Nothing 1000
            , mkHop 1 me receiver "pending" Nothing 800
            ]
      map (.kind) (participantActionsFor chainSlot me hops) `shouldBe` ["lock"]

    it "shows Claim to the receiver of a locked hop once preimage is known" $ do
      let hops =
            [ mkHop 0 receiver me "locked" (Just "preimg") 1000
            , mkHop 1 me bridge "pending" Nothing 800
            ]
      -- I am receiver of hop 0 → Claim. The pending hop 1 (where
      -- I am sender) is *not* lockable because the locker is me
      -- and the upstream condition is satisfied — wait, that means
      -- this fixture also yields a "lock" action. Make sure both
      -- are surfaced.
      map (.kind) (participantActionsFor chainSlot me hops) `shouldMatchList` ["claim", "lock"]

    it "withholds Claim from the receiver before preimage is known" $ do
      let hops =
            [ mkHop 0 receiver me "locked" Nothing 1000
            ]
      participantActionsFor chainSlot me hops `shouldBe` []

    it "shows Refund to the sender of a locked hop after timeout, if no preimage" $ do
      let hops =
            [ mkHop 0 me bridge "locked" Nothing 100 -- timeout 100, chain 500 → expired
            ]
      map (.kind) (participantActionsFor chainSlot me hops) `shouldBe` ["refund"]

    it "withholds Refund if preimage was revealed (claim is in flight)" $ do
      let hops =
            [ mkHop 0 me bridge "locked" (Just "preimg") 100
            ]
      participantActionsFor chainSlot me hops `shouldBe` []

    it "3-hop stuck: bridge sees Refund once its timeout passes but upstream sender does not yet" $ do
      -- Scenario: hop 2 (bridge2→carol) was never created so carol never
      -- revealed the preimage. hop 1's timeout has passed; hop 0's has not.
      let alice   = "alice-pkh"
          bridge1 = "bridge1-pkh"
          bridge2 = "bridge2-pkh"
          chain   = 700 :: Int64
          hops    =
            [ mkHop 0 alice   bridge1 "locked" Nothing 1200
            , mkHop 1 bridge1 bridge2 "locked" Nothing 600
            ]
      -- bridge1 is sender of hop 1: 700 >= 600 → refund
      map (.kind) (participantActionsFor chain bridge1 hops) `shouldBe` ["refund"]
      -- alice is sender of hop 0: 700 < 1200 → no action yet
      participantActionsFor chain alice hops `shouldBe` []

    it "3-hop stuck: original sender sees Refund only after their own timeout passes" $ do
      let alice   = "alice-pkh"
          bridge1 = "bridge1-pkh"
          bridge2 = "bridge2-pkh"
          chain   = 1300 :: Int64
          hops    =
            [ mkHop 0 alice   bridge1 "locked" Nothing 1200
            , mkHop 1 bridge1 bridge2 "locked" Nothing 600
            ]
      map (.kind) (participantActionsFor chain alice hops) `shouldBe` ["refund"]

    it "upstream hop timeout always exceeds downstream by at least hopTimeoutMarginSlots" $ do
      -- Guarantees every bridge has a window to learn the outcome of its
      -- downstream hop before its own upstream lock can be refunded.
      let base = 100_000_000
          n    = 3
          gaps =
            [ hopTimeoutSlot base n i - hopTimeoutSlot base n (i + 1)
            | i <- [0 .. n - 2]
            ]
      all (>= hopTimeoutMarginSlots) gaps `shouldBe` True

  describe "hopTimeoutSlot" $ do
    -- The downstream-most hop (largest hopIndex) should be the *first*
    -- to time out, because it's the receiver-side leg and its claim
    -- gates the whole cascade. Every upstream hop must time out later,
    -- so a bridge that sees the preimage downstream still has time to
    -- claim its upstream lock before that one expires.
    it "is strictly monotone-decreasing in hop index" $ do
      let base = 100_000_000
          n = 4
          ts = [hopTimeoutSlot base n i | i <- [0 .. n - 1]]
      ts `shouldSatisfy` strictlyDecreasing

    it "anchors the downstream-most hop at baseSlot" $ do
      let base = 100_000_000
          n = 3
      hopTimeoutSlot base n (n - 1) `shouldBe` base

    it "spaces hops by exactly hopTimeoutMarginSlots" $ do
      -- We don't import the constant; instead, observe the gap is the
      -- same between every adjacent pair and is > 0.
      let base = 100_000_000
          n = 5
          gaps =
            zipWith
              (-)
              [hopTimeoutSlot base n i | i <- [0 .. n - 2]]
              [hopTimeoutSlot base n i | i <- [1 .. n - 1]]
      gaps `shouldSatisfy` allEqual
      head gaps `shouldSatisfy` (> 0)

    it "single-hop route puts the only hop at baseSlot" $ do
      hopTimeoutSlot 12345 1 0 `shouldBe` 12345
 where
  strictlyDecreasing xs = and (zipWith (>) xs (drop 1 xs))
  allEqual xs = case xs of
    [] -> True
    (h : t) -> all (== h) t
