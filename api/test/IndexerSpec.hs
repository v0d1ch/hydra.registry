module IndexerSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import Db qualified
import Db.Schema (RouteHop (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraEvent (..), HydraUtxoEntry (..))
import Hydra.Htlc qualified as Htlc
import Indexer (processEvent)
import Logging (LogLevel (..), newLogger)
import Relay.EventBus qualified as Bus
import Test.Hspec
import TestUtils (withTestPool)

testPaymentHash :: Text
testPaymentHash = "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"

testScriptAddress :: Text
testScriptAddress = case Htlc.htlcScriptAddress "Preview" of
  Right a -> a
  Left e -> error $ "test setup: " <> show e

setupLockedHop :: Pool -> Text -> Text -> IO ()
setupLockedHop pool headId htlcTxHash = do
  now <- getCurrentTime
  let expiresAt = addUTCTime 3600 now
  Db.upsertHead pool headId "localhost" 4001 "Open" Nothing
  Db.insertInvoice pool "inv-idx" headId "addr_bob" testPaymentHash 50000000 Nothing "pending" expiresAt
  Db.insertPaymentRoute
    pool
    "route-idx"
    "inv-idx"
    "addr_alice"
    "addr_bob"
    50000000
    "in_progress"
    (Aeson.toJSON ([] :: [Aeson.Value]))
    500000
    "Preview"
  Db.insertRouteHops
    pool
    [ ( "hop-idx"
      , "route-idx"
      , 0
      , headId
      , "addr_bridge"
      , "addr_alice"
      , "addr_bridge"
      , "pending"
      , testPaymentHash
      , 9999999999 -- far future: an observed spend classifies as a claim
      , 500000
      )
    ]
  Db.updateHopLocked pool "hop-idx" htlcTxHash now

spec :: Spec
spec = describe "Indexer.HeadFinalized" $ around withTestPool $ do
  let logger = newLogger Info
      scriptHash = Htlc.htlcScriptHashHex

  it "marks hop claimed when HTLC UTxO was settled on L2 before head closed" $ \pool -> do
    setupLockedHop pool "head-fin-A" "lock-tx-fin"
    chainSlotVar <- newTVarIO (0 :: Int64)
    bus <- Bus.newEventBus
    -- Empty finalized UTxOs: HTLC was already spent (claimed or refunded) on L2
    processEvent logger pool chainSlotVar bus (Just scriptHash)
      HeadFinalized{finalizedHeadId = "head-fin-A", finalizedUtxos = []}
    hops <- Db.getRouteHops pool "route-idx"
    let hop = head hops
    hop.hopHtlcStatus `shouldBe` "claimed"

  it "leaves hop locked when HTLC UTxO survived to L1 fanout" $ \pool -> do
    setupLockedHop pool "head-fin-B" "lock-tx-fin"
    chainSlotVar <- newTVarIO (0 :: Int64)
    bus <- Bus.newEventBus
    -- HTLC UTxO still present in finalized set: it landed on L1 unsettled
    let htlcUtxo =
          HydraUtxoEntry
            { txHash = "lock-tx-fin"
            , outputIndex = 0
            , address = testScriptAddress
            , lovelace = 50000000
            , nativeAssets = Map.empty
            , datumHash = Nothing
            , inlineDatum = Nothing
            , referenceScript = Nothing
            }
    processEvent logger pool chainSlotVar bus (Just scriptHash)
      HeadFinalized{finalizedHeadId = "head-fin-B", finalizedUtxos = [htlcUtxo]}
    hops <- Db.getRouteHops pool "route-idx"
    let hop = head hops
    hop.hopHtlcStatus `shouldBe` "locked"
