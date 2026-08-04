-- | Track HTLC settlements on L1.
--
-- The L2 HTLC watcher goes blind the moment a head closes with a hop in
-- flight: fanout drops the HTLC UTxO onto L1 and the claim or refund
-- settles there. This scan polls the HTLC script address on each
-- configured network through the same local cardano-node sockets the
-- head scan uses, matches fanned-out HTLCs back to route hops via their
-- datum, and resolves the hop when the UTxO disappears.
--
-- Polling a node's UTxO set can never see the /spending/ transaction
-- (and therefore neither the redeemer nor a claim's preimage), so
-- claim-vs-refund is classified by timing: the validator's validity
-- windows are disjoint around the datum timeout - claims must complete
-- strictly before it, refunds strictly after - so the spend window
-- @(lastSeenSlot, tipSlot]@ decides the outcome whenever it does not
-- straddle the timeout. Straddling windows stay @locked@ and warn;
-- redeemer-level resolution (and preimage recovery) needs a chain-sync
-- follower and is deliberately out of scope here.
module L1.HtlcScan where

import Control.Exception (SomeException, try)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Time (getCurrentTime)
import Db qualified
import Db.Schema (RouteHop (..))
import GHC.IsList (toList)
import Hasql.Pool (Pool)
import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient (QueryPoint (..), cardanoModeParams, queryTip, queryUTxO)
import Hydra.Htlc qualified as Htlc
import L1.HeadScan (networkIdFor)
import Logging (Logger, logInfo, logWarn)
import Relay.EventBus (EventBus, RouteEvent (..))
import Relay.EventBus qualified as Bus
import Relay.HtlcWatcher (Settlement (..), checkRouteCompletion, classifySettlement)

-- | One HTLC UTxO sitting at the script address, identified by its
-- decoded datum. Hops are matched on secret hash /and/ receiver - every
-- hop of a route shares the hash, and fanout re-creates outputs under
-- the fanout txid, so the datum is the only stable key.
data HtlcObservation = HtlcObservation
  { obsSecretHash :: Text
  , obsSenderPkh :: Text
  , obsReceiverPkh :: Text
  }
  deriving stock (Eq, Show)

-- | Decode an inline HTLC datum from a UTxO at the script address.
observationOf :: TxOut CtxUTxO -> Maybe HtlcObservation
observationOf (TxOut _ _ datum _) = case datum of
  TxOutDatumInline sd -> do
    d <- Htlc.decodeDatumCbor (serialiseToCBOR sd)
    pure
      HtlcObservation
        { obsSecretHash = d.datumHashHex
        , obsSenderPkh = d.datumSenderHex
        , obsReceiverPkh = d.datumReceiverHex
        }
  _ -> Nothing

-- | Poll one network's HTLC script address. Returns 'Nothing' on any
-- failure - the caller must NOT treat a failed scan as an empty
-- observation set, or every in-flight hop would look spent.
scanNetwork :: Logger -> Text -> Text -> FilePath -> IO (Maybe (Int64, [HtlcObservation]))
scanNetwork logger htlcHash network socketPath =
  case (networkIdFor network, Htlc.scriptAddressFromHash htlcHash network) of
    (Nothing, _) -> do
      logWarn logger "L1 HTLC scan: unknown network" [("network", toJSON network)]
      pure Nothing
    (_, Left err) -> do
      logWarn logger "L1 HTLC scan: bad script address" [("error", toJSON err)]
      pure Nothing
    (Just networkId, Right addrText) ->
      case deserialiseAddress (AsAddress AsShelleyAddr) addrText of
        Nothing -> do
          logWarn logger "L1 HTLC scan: address deserialisation failed" [("address", toJSON addrText)]
          pure Nothing
        Just addr -> do
          let connectInfo = LocalNodeConnectInfo cardanoModeParams networkId (File socketPath)
          result <- try @SomeException $ do
            tip <- queryTip connectInfo
            utxo <- queryUTxO connectInfo QueryTip [addr]
            pure (tip, utxo)
          case result of
            Left err -> do
              logWarn logger "L1 HTLC scan failed" [("network", toJSON network), ("error", toJSON (show err))]
              pure Nothing
            Right (tip, utxo) -> do
              let tipSlot = case tip of
                    ChainPointAtGenesis -> 0
                    ChainPoint (SlotNo s) _ -> fromIntegral s
                  obs = mapMaybe (observationOf . snd) (toList utxo)
              pure $ Just (tipSlot, obs)

-- | Reconcile one scan's observations against all locked hops:
--
--   * hop's HTLC present at the script address → stamp the tip slot
--     onto @htlc_last_seen_slot@ (shared with the L2 watcher's stamps)
--   * hop absent but previously seen → the UTxO was spent; classify by
--     timing and mark the hop claimed or refunded (events included)
--   * hop absent and never seen on L1 → nothing to conclude here (it
--     may still live inside an open head; the L2 watcher owns that
--     case and applies the same classification)
applyHtlcScan :: Logger -> Pool -> EventBus -> Int64 -> [HtlcObservation] -> IO ()
applyHtlcScan logger pool bus tipSlot observations = do
  locked <- Db.getLockedHops pool
  mapM_ step locked
 where
  normalizePkh t = either (const Nothing) (Just . Htlc.hexEncode) (Htlc.addressOrPkhToBytes t)

  matches h o =
    o.obsSecretHash == h.hopSecretHash
      && normalizePkh h.hopReceiverAddress == Just o.obsReceiverPkh

  step h
    | any (matches h) observations = Db.updateHopHtlcLastSeen pool h.hopId tipSlot
    | otherwise = case h.hopHtlcLastSeenSlot of
        Nothing -> pure ()
        Just lastSeen -> settle h lastSeen

  settle h lastSeen = case classifySettlement tipSlot lastSeen h.hopTimeoutSlot of
    SettledClaimed -> do
      now <- getCurrentTime
      logInfo logger "L1 HTLC claim detected" [("hopId", toJSON h.hopId), ("routeId", toJSON h.hopRouteId)]
      Db.updateHopClaimed pool h.hopId now
      Bus.publish bus HopClaimed{routeId = h.hopRouteId, hopIndex = fromIntegral h.hopIndex, at = now}
      checkRouteCompletion logger pool bus h.hopRouteId
    SettledRefunded -> do
      now <- getCurrentTime
      logInfo logger "L1 HTLC refund detected" [("hopId", toJSON h.hopId), ("routeId", toJSON h.hopRouteId)]
      Db.updateHopRefunded pool h.hopId
      Bus.publish bus HopRefunded{routeId = h.hopRouteId, hopIndex = fromIntegral h.hopIndex, at = now}
    SettlementAmbiguous ->
      logWarn
        logger
        "L1 HTLC spend window straddles the timeout - leaving hop locked"
        [ ("hopId", toJSON h.hopId)
        , ("lastSeen", toJSON lastSeen)
        , ("tip", toJSON tipSlot)
        , ("timeout", toJSON h.hopTimeoutSlot)
        ]
