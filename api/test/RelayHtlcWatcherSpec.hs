module RelayHtlcWatcherSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Time (addUTCTime, getCurrentTime)
import Db qualified
import Db.Schema (Invoice (..), PaymentRoute (..), RouteHop (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraUtxoEntry (..))
import Logging (LogLevel (..), newLogger)
import Relay.HtlcWatcher qualified as HtlcWatcher
import Test.Hspec
import TestUtils

-- | The HTLC script hash used in tests
testScriptHash :: Text
testScriptHash = "81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df"

-- | An address that embeds the test script hash (mimics a script address)
testScriptAddress :: Text
testScriptAddress = "addr1w" <> testScriptHash <> "rest"

testPaymentHash :: Text
testPaymentHash = "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"

-- | Create test invoice, route, and hops for a given head
setupTestPayment :: Pool -> Text -> Text -> Text -> IO ()
setupTestPayment pool headId senderAddr receiverAddr = do
  now <- getCurrentTime
  let expiresAt = addUTCTime 3600 now
  Db.insertInvoice pool "inv-1" receiverAddr testPaymentHash 50000000 Nothing "pending" expiresAt
  Db.insertPaymentRoute pool "route-1" "inv-1" senderAddr receiverAddr 50000000 "in_progress" (Aeson.toJSON ([] :: [Value])) 500000 "Preview"
  Db.insertRouteHops
    pool
    [ ("hop-1", "route-1", 0, headId, "addr_bridge", senderAddr, "addr_bridge", "pending", testPaymentHash, 1000000, 500000)
    , ("hop-2", "route-1", 1, headId, "addr_bridge", "addr_bridge", receiverAddr, "pending", testPaymentHash, 1000000, 500000)
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

spec :: Spec
spec = describe "Relay.HtlcWatcher" $ around withTestPool $ do
  let logger = newLogger Info

  describe "processUtxoSnapshot — lock detection" $ do
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
      HtlcWatcher.processUtxoSnapshot logger pool "head-A" testScriptHash [htlcUtxo]

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
      HtlcWatcher.processUtxoSnapshot logger pool "head-A" testScriptHash [unrelatedUtxo]

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
      HtlcWatcher.processUtxoSnapshot logger pool "head-B" testScriptHash [htlcUtxo]

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "pending"

  describe "processUtxoSnapshot — claim detection" $ do
    it "detects a claim when locked HTLC UTxO disappears" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      -- First lock a hop
      now <- getCurrentTime
      Db.updateHopLocked pool "hop-1" "lock-tx-001" now

      -- Snapshot without the locked UTxO — it was spent (claimed)
      HtlcWatcher.processUtxoSnapshot logger pool "head-A" testScriptHash []

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "claimed"
      hop1.hopClaimedAt `shouldSatisfy` (/= Nothing)

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
      HtlcWatcher.processUtxoSnapshot logger pool "head-A" testScriptHash [stillLockedUtxo]

      hops <- Db.getRouteHops pool "route-1"
      let hop1 = head $ filter (\h -> h.hopId == "hop-1") hops
      hop1.hopHtlcStatus `shouldBe` "locked"

  describe "processUtxoSnapshot — payment completion" $ do
    it "marks route completed and invoice paid when all hops claimed" $ \pool -> do
      setupTestPayment pool "head-A" "addr_alice" "addr_bob"

      -- Lock both hops
      now <- getCurrentTime
      Db.updateHopLocked pool "hop-1" "lock-tx-001" now
      Db.updateHopLocked pool "hop-2" "lock-tx-002" now

      -- Claim hop-1 (UTxO disappears)
      HtlcWatcher.processUtxoSnapshot logger pool "head-A" testScriptHash [HydraUtxoEntry "lock-tx-002" 0 testScriptAddress 50000000 Map.empty Nothing (Just (mkHtlcDatum testPaymentHash)) Nothing]

      -- Hop-1 claimed, hop-2 still locked
      hops1 <- Db.getRouteHops pool "route-1"
      let h1 = head $ filter (\h -> h.hopId == "hop-1") hops1
      h1.hopHtlcStatus `shouldBe` "claimed"

      route1 <- Db.getPaymentRoute pool "route-1"
      case route1 of
        Just r -> r.routeStatus `shouldBe` "in_progress" -- not yet complete
        Nothing -> expectationFailure "Route not found"

      -- Claim hop-2 (all UTxOs gone)
      HtlcWatcher.processUtxoSnapshot logger pool "head-A" testScriptHash []

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
      Db.insertInvoice pool "inv-expired" "addr_bob" "somehash" 1000000 Nothing "pending" expiredTime

      Db.expirePendingInvoices pool now

      mInv <- Db.getInvoice pool "inv-expired"
      case mInv of
        Just inv -> inv.invoiceStatus `shouldBe` "expired"
        Nothing -> expectationFailure "Invoice not found"

    it "does not expire invoices that are still valid" $ \pool -> do
      now <- getCurrentTime
      let futureTime = addUTCTime 3600 now
      Db.insertInvoice pool "inv-valid" "addr_bob" "somehash" 1000000 Nothing "pending" futureTime

      Db.expirePendingInvoices pool now

      mInv <- Db.getInvoice pool "inv-valid"
      case mInv of
        Just inv -> inv.invoiceStatus `shouldBe` "pending"
        Nothing -> expectationFailure "Invoice not found"

    it "expires routes whose invoices have expired" $ \pool -> do
      now <- getCurrentTime
      let expiredTime = addUTCTime (-60) now
      Db.insertInvoice pool "inv-exp" "addr_bob" "somehash" 1000000 Nothing "pending" expiredTime
      Db.insertPaymentRoute pool "route-exp" "inv-exp" "addr_alice" "addr_bob" 1000000 "in_progress" (Aeson.toJSON ([] :: [Value])) 0 "Preview"

      -- First expire the invoice
      Db.expirePendingInvoices pool now
      -- Then expire stale routes
      Db.expireStaleRoutes pool now

      mRoute <- Db.getPaymentRoute pool "route-exp"
      case mRoute of
        Just r -> r.routeStatus `shouldBe` "expired"
        Nothing -> expectationFailure "Route not found"
