module RelayHtlcWatcherSpec (spec) where

import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Time (addUTCTime, getCurrentTime)
import Db qualified
import Db.Schema (Invoice (..), PaymentRoute (..), RouteHop (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraUtxoEntry (..))
import Hydra.Htlc qualified as Htlc
import Logging (LogLevel (..), newLogger)
import Relay.EventBus (RouteEvent (..))
import Relay.EventBus qualified as Bus
import Relay.HtlcWatcher qualified as HtlcWatcher
import Test.Hspec
import TestUtils

-- | The HTLC script hash used in tests - same as the production constant
-- so the bech32 derivation in the watcher round-trips.
testScriptHash :: Text
testScriptHash = Htlc.htlcScriptHashHex

-- | The real bech32 Preview script address derived from the test hash.
-- The watcher does string equality against this; constructing fakes via
-- substring concatenation no longer works (and that's the regression we
-- guard against below).
testScriptAddress :: Text
testScriptAddress = case Htlc.htlcScriptAddress "Preview" of
  Right a -> a
  Left e -> error $ "test setup: " <> show e

testPaymentHash :: Text
testPaymentHash = "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"

-- | Timeout slot far in the future on every network - spends detected
-- while the clock is before this slot classify as claims.
futureTimeout :: Int64
futureTimeout = 9_999_999_999

-- | Create test invoice, route, and hops for a given head. The timeout
-- is far in the future so in-head spends read as claims; refund-path
-- tests seed their own hops with a past timeout.
setupTestPayment :: Pool -> Text -> Text -> Text -> IO ()
setupTestPayment pool headId senderAddr receiverAddr = do
  now <- getCurrentTime
  let expiresAt = addUTCTime 3600 now
  Db.insertInvoice pool "inv-1" "test-head" receiverAddr testPaymentHash 50000000 Nothing "pending" expiresAt
  Db.insertPaymentRoute pool "route-1" "inv-1" senderAddr receiverAddr 50000000 "in_progress" (Aeson.toJSON ([] :: [Value])) 500000 "Preview"
  Db.insertRouteHops
    pool
    [ ("hop-1", "route-1", 0, headId, "addr_bridge", senderAddr, "addr_bridge", "pending", testPaymentHash, futureTimeout, 500000)
    , ("hop-2", "route-1", 1, headId, "addr_bridge", "addr_bridge", receiverAddr, "pending", testPaymentHash, futureTimeout, 500000)
    ]

-- | Build an inline datum with a payment hash (Plutus JSON encoding)
mkHtlcDatum :: Text -> Value
mkHtlcDatum payHash =
  Aeson.Object $
    KM.fromList
      [ ("constructor", Aeson.Number 0)
      ,
        ( "fields"
        , Aeson.Array $
            Vector.fromList
              [ Aeson.Object $ KM.singleton "bytes" (Aeson.String payHash)
              , Aeson.Object $ KM.singleton "int" (Aeson.Number 1000000)
              , Aeson.Object $ KM.singleton "bytes" (Aeson.String "sender_pkh")
              , Aeson.Object $ KM.singleton "bytes" (Aeson.String "receiver_pkh")
              ]
        )
      ]

-- | Drop-in wrapper so existing tests can keep their old signature
-- (no explicit bus). A throwaway 'EventBus' is created per call -
-- callers don't read from it.
runWatcher :: Pool -> Text -> Text -> [HydraUtxoEntry] -> IO ()
runWatcher pool headId scriptHash utxos = do
  bus <- Bus.newEventBus
  HtlcWatcher.processUtxoSnapshot (newLogger Info) pool bus headId scriptHash utxos

spec :: Spec
spec = describe "Relay.HtlcWatcher" $ around withTestPool $ do
  describe "processUtxoSnapshot - lock detection" $ do
    it "detects a new HTLC lock and updates hop status" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      -- Simulate a snapshot with an HTLC UTxO
      let htlcUtxo =
            HydraUtxoEntry
              { txHash = "lock-tx-001"
              , outputIndex = 0
              , address = testScriptAddress
              , lovelace = 50000000
              , nativeAssets = Map.empty
              , datumHash = Nothing
              , inlineDatum = Just (mkHtlcDatum testPaymentHash)
              , referenceScript = Nothing
              }
      runWatcher pool "head-A" testScriptHash [htlcUtxo]

      -- Check that hop-1 is now locked
      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "locked"
      hop1.hopHtlcTxHash `shouldBe` Just "lock-tx-001"
      hop1.hopLockedAt `shouldSatisfy` (/= Nothing)

    it "does not lock hops for unrelated payment hashes" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      let unrelatedUtxo =
            HydraUtxoEntry
              { txHash = "lock-tx-002"
              , outputIndex = 0
              , address = testScriptAddress
              , lovelace = 50000000
              , nativeAssets = Map.empty
              , datumHash = Nothing
              , inlineDatum = Just (mkHtlcDatum "0000000000000000000000000000000000000000000000000000000000000000")
              , referenceScript = Nothing
              }
      runWatcher pool "head-A" testScriptHash [unrelatedUtxo]

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "pending"

    it "does not lock hops for a different head" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      let htlcUtxo =
            HydraUtxoEntry
              { txHash = "lock-tx-003"
              , outputIndex = 0
              , address = testScriptAddress
              , lovelace = 50000000
              , nativeAssets = Map.empty
              , datumHash = Nothing
              , inlineDatum = Just (mkHtlcDatum testPaymentHash)
              , referenceScript = Nothing
              }
      -- Process for head-B, but hops are in head-A
      runWatcher pool "head-B" testScriptHash [htlcUtxo]

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "pending"

    -- Regression for a bug observed in the manual e2e on 2026-04-30:
    -- the watcher used to do a substring check of the *hex* script hash
    -- against the bech32 address, which is structurally impossible to
    -- match (bech32 uses base32, not hex). The old check tolerated
    -- synthetic test addresses where the hash happened to appear in the
    -- string verbatim, masking the bug. With the equality-on-bech32
    -- check, an address that merely embeds the hex hash but isn't the
    -- real derived script address is correctly rejected.
    it "does not match an address that merely embeds the hex hash" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      let fakeAddr = "addr1w" <> testScriptHash <> "rest"
          fakeUtxo =
            HydraUtxoEntry
              { txHash = "lock-tx-fake"
              , outputIndex = 0
              , address = fakeAddr
              , lovelace = 50000000
              , nativeAssets = Map.empty
              , datumHash = Nothing
              , inlineDatum = Just (mkHtlcDatum testPaymentHash)
              , referenceScript = Nothing
              }
      runWatcher pool "head-A" testScriptHash [fakeUtxo]

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "pending"

    it "matches the real bech32 script address on Mainnet too" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"
      mainnetAddr <- case Htlc.htlcScriptAddress "Mainnet" of
        Right a -> pure a
        Left e -> expectationFailure (show e) >> error "unreachable"

      let mainnetUtxo =
            HydraUtxoEntry
              { txHash = "lock-tx-mainnet"
              , outputIndex = 0
              , address = mainnetAddr
              , lovelace = 50000000
              , nativeAssets = Map.empty
              , datumHash = Nothing
              , inlineDatum = Just (mkHtlcDatum testPaymentHash)
              , referenceScript = Nothing
              }
      runWatcher pool "head-A" testScriptHash [mainnetUtxo]

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "locked"

  describe "EventBus fan-out" $ do
    -- These tests pin the contract that "the watcher updates the
    -- DB *and* publishes a corresponding RouteEvent" - they're what
    -- the SSE endpoint relies on to push pushes to subscribers.
    it "publishes HopLocked when a lock is detected" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"
      bus <- Bus.newEventBus
      sub <- Bus.subscribe bus
      let htlcUtxo =
            HydraUtxoEntry
              { txHash = "lock-tx-evt"
              , outputIndex = 0
              , address = testScriptAddress
              , lovelace = 50000000
              , nativeAssets = Map.empty
              , datumHash = Nothing
              , inlineDatum = Just (mkHtlcDatum testPaymentHash)
              , referenceScript = Nothing
              }
      HtlcWatcher.processUtxoSnapshot
        (newLogger Info)
        pool
        bus
        "head-A"
        testScriptHash
        [htlcUtxo]
      ev <- atomically (readTChan sub)
      case ev of
        HopLocked{routeId, hopIndex, txHash} -> do
          routeId `shouldBe` "route-1"
          hopIndex `shouldBe` 0
          txHash `shouldBe` "lock-tx-evt"
        other -> expectationFailure ("expected HopLocked, got " <> show other)

    it "publishes HopClaimed when a previously locked UTxO disappears" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"
      now <- getCurrentTime
      Db.updateHopLocked pool "hop-1" "lock-tx-001" now
      bus <- Bus.newEventBus
      sub <- Bus.subscribe bus
      HtlcWatcher.processUtxoSnapshot
        (newLogger Info)
        pool
        bus
        "head-A"
        testScriptHash
        []
      ev <- atomically (readTChan sub)
      case ev of
        HopClaimed{routeId, hopIndex} -> do
          routeId `shouldBe` "route-1"
          hopIndex `shouldBe` 0
        other -> expectationFailure ("expected HopClaimed, got " <> show other)

    it "publishes RouteCompleted after every hop is claimed" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"
      now <- getCurrentTime
      Db.updateHopLocked pool "hop-1" "tx-1" now
      Db.updateHopLocked pool "hop-2" "tx-2" now
      bus <- Bus.newEventBus
      sub <- Bus.subscribe bus
      -- First snapshot still has hop-2's UTxO present, so only
      -- hop-1's claim is detected and the route is not yet
      -- complete.
      HtlcWatcher.processUtxoSnapshot
        (newLogger Info)
        pool
        bus
        "head-A"
        testScriptHash
        [ HydraUtxoEntry
            "tx-2"
            0
            testScriptAddress
            50000000
            Map.empty
            Nothing
            (Just (mkHtlcDatum testPaymentHash))
            Nothing
        ]
      _ <- atomically (readTChan sub) -- HopClaimed for hop-1
      -- Second snapshot has nothing → hop-2 is also claimed →
      -- RouteCompleted should follow.
      HtlcWatcher.processUtxoSnapshot
        (newLogger Info)
        pool
        bus
        "head-A"
        testScriptHash
        []
      _ <- atomically (readTChan sub) -- HopClaimed for hop-2
      finalEv <- atomically (readTChan sub)
      case finalEv of
        RouteCompleted{routeId} -> routeId `shouldBe` "route-1"
        other -> expectationFailure ("expected RouteCompleted, got " <> show other)

    it "does not publish anything when the watcher does no-op work" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"
      bus <- Bus.newEventBus
      sub <- Bus.subscribe bus
      -- Empty snapshot, no locked hops in DB → nothing to detect.
      HtlcWatcher.processUtxoSnapshot
        (newLogger Info)
        pool
        bus
        "head-A"
        testScriptHash
        []
      empty <- atomically (isEmptyTChan sub)
      empty `shouldBe` True

  describe "processUtxoSnapshot - claim detection" $ do
    it "detects a claim when locked HTLC UTxO disappears" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      -- First lock a hop
      now <- getCurrentTime
      Db.updateHopLocked pool "hop-1" "lock-tx-001" now

      -- Snapshot without the locked UTxO - it was spent (claimed)
      runWatcher pool "head-A" testScriptHash []

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "claimed"
      hop1.hopClaimedAt `shouldSatisfy` (/= Nothing)

    it "classifies a spend after the timeout as refunded, not claimed" $ \pool -> do
      -- After the timeout passes only the sender's Refund tx can spend
      -- the HTLC (the validator's validity windows are disjoint around
      -- the timeout). A spend observed while the clock is already past
      -- the timeout must therefore be a refund - marking it claimed
      -- would complete routes and pay invoices that actually failed.
      now <- getCurrentTime
      let expiresAt = addUTCTime 3600 now
      Db.insertInvoice pool "inv-r" "test-head" "addr_bob" testPaymentHash 1000000 Nothing "pending" expiresAt
      Db.insertPaymentRoute pool "route-r" "inv-r" "addr_alice" "addr_bob" 1000000 "in_progress" (Aeson.toJSON ([] :: [Value])) 0 "Preview"
      Db.insertRouteHops
        pool
        -- timeout slot 1_000_000 is long past on Preview
        [("hop-r", "route-r", 0, "head-R", "addr_bridge", "addr_alice", "addr_bob", "pending", testPaymentHash, 1000000, 0)]
      Db.updateHopLocked pool "hop-r" "lock-tx-refund" now
      -- Empty snapshot: the HTLC UTxO was spent - necessarily by Refund
      runWatcher pool "head-R" testScriptHash []
      hops <- Db.getRouteHops pool "route-r"
      map (.hopHtlcStatus) hops `shouldBe` ["refunded"]
      -- The route must NOT be completed and the invoice NOT paid
      mRoute <- Db.getPaymentRoute pool "route-r"
      fmap (.routeStatus) mRoute `shouldBe` Just "in_progress"

    it "does not claim when locked HTLC UTxO is still present" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      now <- getCurrentTime
      Db.updateHopLocked pool "hop-1" "lock-tx-001" now

      -- Snapshot WITH the locked UTxO still present
      let stillLockedUtxo =
            HydraUtxoEntry
              { txHash = "lock-tx-001"
              , outputIndex = 0
              , address = testScriptAddress
              , lovelace = 50000000
              , nativeAssets = Map.empty
              , datumHash = Nothing
              , inlineDatum = Just (mkHtlcDatum testPaymentHash)
              , referenceScript = Nothing
              }
      runWatcher pool "head-A" testScriptHash [stillLockedUtxo]

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "locked"

  describe "processUtxoSnapshot - payment completion" $ do
    it "marks route completed and invoice paid when all hops claimed" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      -- Lock both hops
      now <- getCurrentTime
      Db.updateHopLocked pool "hop-1" "lock-tx-001" now
      Db.updateHopLocked pool "hop-2" "lock-tx-002" now

      -- Claim hop-1 (UTxO disappears)
      runWatcher pool "head-A" testScriptHash [HydraUtxoEntry "lock-tx-002" 0 testScriptAddress 50000000 Map.empty Nothing (Just (mkHtlcDatum testPaymentHash)) Nothing]

      -- Hop-1 claimed, hop-2 still locked
      hops1 <- Db.getRouteHops pool "route-1"
      let h1 = head $ filter (\h -> h.hopId == "hop-1") hops1
      h1.hopHtlcStatus `shouldBe` "claimed"

      route1 <- Db.getPaymentRoute pool "route-1"
      case route1 of
        Just r -> r.routeStatus `shouldBe` "in_progress" -- not yet complete
        Nothing -> expectationFailure "Route not found"

      -- Claim hop-2 (all UTxOs gone)
      runWatcher pool "head-A" testScriptHash []

      -- Now both claimed → route completed, invoice paid
      hops2 <- Db.getRouteHops pool "route-1"
      let h2 = head $ filter (\h -> h.hopId == "hop-2") hops2
      h2.hopHtlcStatus `shouldBe` "claimed"

      route2 <- Db.getPaymentRoute pool "route-1"
      case route2 of
        Just r -> r.routeStatus `shouldBe` "completed"
        Nothing -> expectationFailure "Route not found"

      invoice <- Db.getInvoice pool "inv-1"
      case invoice of
        Just inv -> inv.invoiceStatus `shouldBe` "paid"
        Nothing -> expectationFailure "Invoice not found"

  describe "preimage submission" $ do
    it "stores preimage on all hops with matching secret hash" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      Db.setPreimageByHash pool testPaymentHash "the-secret-preimage"

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
          hop2 = head $ filter (\h -> h.hopId == "hop-2") hops
      hop1.hopPreimage `shouldBe` Just "the-secret-preimage"
      hop2.hopPreimage `shouldBe` Just "the-secret-preimage"

  describe "expiry" $ do
    it "expires pending invoices past their deadline" $ \pool -> do
      now <- getCurrentTime
      let expiredTime = addUTCTime (-60) now -- expired 60 seconds ago
      Db.insertInvoice pool "inv-expired" "test-head" "addr_bob" "somehash" 1000000 Nothing "pending" expiredTime

      Db.expirePendingInvoices pool now

      mInv <- Db.getInvoice pool "inv-expired"
      case mInv of
        Just inv -> inv.invoiceStatus `shouldBe` "expired"
        Nothing -> expectationFailure "Invoice not found"

    it "does not expire invoices that are still valid" $ \pool -> do
      now <- getCurrentTime
      let futureTime = addUTCTime 3600 now
      Db.insertInvoice pool "inv-valid" "test-head" "addr_bob" "somehash" 1000000 Nothing "pending" futureTime

      Db.expirePendingInvoices pool now

      mInv <- Db.getInvoice pool "inv-valid"
      case mInv of
        Just inv -> inv.invoiceStatus `shouldBe` "pending"
        Nothing -> expectationFailure "Invoice not found"

    it "expires routes whose invoices have expired" $ \pool -> do
      now <- getCurrentTime
      let expiredTime = addUTCTime (-60) now
      Db.insertInvoice pool "inv-exp" "test-head" "addr_bob" "somehash" 1000000 Nothing "pending" expiredTime
      Db.insertPaymentRoute pool "route-exp" "inv-exp" "addr_alice" "addr_bob" 1000000 "in_progress" (Aeson.toJSON ([] :: [Value])) 0 "Preview"

      -- First expire the invoice
      Db.expirePendingInvoices pool now
      -- Then expire stale routes
      Db.expireStaleRoutes pool now

      mRoute <- Db.getPaymentRoute pool "route-exp"
      case mRoute of
        Just r -> r.routeStatus `shouldBe` "expired"
        Nothing -> expectationFailure "Route not found"
