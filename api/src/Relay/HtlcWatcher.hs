module Relay.HtlcWatcher
  ( processUtxoSnapshot
  , htlcScriptAddresses
  , checkRouteCompletion
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Either (rights)
import Data.Functor.Identity (Identity)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Db qualified
import Db.Schema (PaymentRoute (..), RouteHop (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraUtxoEntry (..))
import Hydra.Htlc qualified as Htlc
import Logging
import Relay.EventBus (EventBus, RouteEvent (..))
import Relay.EventBus qualified as Bus

-- | After each snapshot, detect HTLC lock and claim events by comparing
-- the new UTxO set against active route hops for this head.
--
-- Lock: a new UTxO at the HTLC script address with an inline datum
--       whose payment hash matches a pending hop → mark hop as 'locked'.
--
-- Claim: a previously locked hop's HTLC tx hash is no longer in the
--        UTxO set (spent) → mark hop as 'claimed'.
--
-- Whenever the DB is updated the watcher also fans out the
-- corresponding 'RouteEvent' on the supplied 'EventBus'. SSE
-- subscribers see the transition without polling.
processUtxoSnapshot :: Logger -> Pool -> EventBus -> Text -> Text -> [HydraUtxoEntry] -> IO ()
processUtxoSnapshot logger pool bus headId htlcScriptHash utxos = do
  -- Get all active (pending or locked) hops for this head
  activeHops <- Db.getActiveHopsByHead pool headId

  -- Detect locks: UTxOs at HTLC script address with matching payment hash.
  -- The address comparison is against the bech32-encoded script address
  -- for every supported network — we don't know the head's network at
  -- watcher time, but the script hash is fixed so the candidate set is
  -- small.
  let scriptAddrs = htlcScriptAddresses htlcScriptHash
      htlcUtxos = filter (isHtlcUtxo scriptAddrs) utxos
  mapM_ (detectLock logger pool bus headId activeHops) htlcUtxos

  -- Detect claims: locked hops whose HTLC tx is no longer in the UTxO set
  let utxoTxHashes = map (\u -> u.txHash) utxos
  mapM_ (detectClaim logger pool bus utxoTxHashes) activeHops

-- | Bech32 enterprise-script addresses for the given script hash on
-- every supported network. Filters out networks where address derivation
-- fails (shouldn't happen for the hard-coded list, but kept tolerant).
htlcScriptAddresses :: Text -> Set Text
htlcScriptAddresses scriptHash =
  Set.fromList $
    rights [Htlc.scriptAddressFromHash scriptHash n | n <- ["Mainnet", "Preview", "Preprod"]]

-- | A UTxO is an HTLC lock candidate when it sits at the bech32 script
-- address for the configured validator on any supported network and has
-- an inline datum we can parse.
isHtlcUtxo :: Set Text -> HydraUtxoEntry -> Bool
isHtlcUtxo addrs entry =
  Set.member entry.address addrs
    && hasInlineDatum entry
 where
  hasInlineDatum e = case e.inlineDatum of
    Just _ -> True
    Nothing -> False

-- | Try to match an HTLC UTxO to a pending hop and mark it as locked.
detectLock :: Logger -> Pool -> EventBus -> Text -> [RouteHop Identity] -> HydraUtxoEntry -> IO ()
detectLock logger pool bus headId activeHops entry = do
  case extractPaymentHash entry.inlineDatum of
    Nothing -> pure ()
    Just payHash -> do
      let matchingHops =
            [ h
            | h <- activeHops
            , h.hopSecretHash == payHash
            , h.hopHtlcStatus == "pending"
            , h.hopHeadId == headId
            ]
      now <- getCurrentTime
      mapM_ (recordLock logger pool bus headId entry now) matchingHops

recordLock
  :: Logger
  -> Pool
  -> EventBus
  -> Text
  -> HydraUtxoEntry
  -> UTCTime
  -> RouteHop Identity
  -> IO ()
recordLock logger pool bus headId entry now h = do
  logInfo
    logger
    "HTLC lock detected"
    [ ("hopId", Aeson.toJSON h.hopId)
    , ("headId", Aeson.toJSON headId)
    , ("txHash", Aeson.toJSON entry.txHash)
    ]
  Db.updateHopLocked pool h.hopId entry.txHash now
  Bus.publish
    bus
    HopLocked
      { routeId = h.hopRouteId
      , hopIndex = fromIntegral h.hopIndex
      , txHash = entry.txHash
      , at = now
      }

-- | Check if a locked hop's HTLC UTxO has been spent (claimed).
detectClaim :: Logger -> Pool -> EventBus -> [Text] -> RouteHop Identity -> IO ()
detectClaim logger pool bus utxoTxHashes hop = do
  case hop.hopHtlcTxHash of
    Nothing -> pure ()
    Just lockedTxHash ->
      -- If the locked tx hash is no longer in the UTxO set, it was spent (claimed)
      if lockedTxHash `notElem` utxoTxHashes && hop.hopHtlcStatus == "locked"
        then do
          now <- getCurrentTime
          logInfo logger "HTLC claim detected" [("hopId", Aeson.toJSON hop.hopId), ("txHash", Aeson.toJSON lockedTxHash)]
          Db.updateHopClaimed pool hop.hopId now
          Bus.publish
            bus
            HopClaimed
              { routeId = hop.hopRouteId
              , hopIndex = fromIntegral hop.hopIndex
              , at = now
              }
          -- Check if all hops in this route are now claimed → complete the payment
          checkRouteCompletion logger pool bus hop.hopRouteId
        else pure ()

-- | Extract the payment hash from an HTLC inline datum.
-- The datum structure is: {"hash": "<hex>", "timeout": ..., "sender": ..., "receiver": ...}
-- or the Plutus JSON encoding: {"fields": [{"bytes": "<hash>"}, ...], "constructor": 0}
extractPaymentHash :: Maybe Value -> Maybe Text
extractPaymentHash Nothing = Nothing
extractPaymentHash (Just val) = case val of
  Aeson.Object obj ->
    -- Try direct field access: {"hash": "abcd..."}
    case KM.lookup "hash" obj of
      Just (Aeson.String h) -> Just h
      _ ->
        -- Try Plutus JSON encoding: {"fields": [{"bytes": "abcd..."}, ...], "constructor": 0}
        case KM.lookup "fields" obj of
          Just (Aeson.Array fields) -> case foldr (:) [] fields of
            (Aeson.Object f : _) -> case KM.lookup "bytes" f of
              Just (Aeson.String h) -> Just h
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
  _ -> Nothing

-- | Check if all hops in a route are claimed. If so, mark the route
-- as completed and the invoice as paid.
checkRouteCompletion :: Logger -> Pool -> EventBus -> Text -> IO ()
checkRouteCompletion logger pool bus rid = do
  hops <- Db.getRouteHops pool rid
  let allClaimed = all (\h -> h.hopHtlcStatus == "claimed") hops
  if allClaimed && not (null hops)
    then do
      logInfo logger "All hops claimed — payment complete" [("routeId", Aeson.toJSON rid)]
      Db.updateRouteStatus pool rid "completed"
      -- Mark the invoice as paid
      mRoute <- Db.getPaymentRoute pool rid
      case mRoute of
        Just route -> Db.updateInvoiceStatus pool route.routeInvoiceId "paid"
        Nothing -> pure ()
      Bus.publish bus RouteCompleted{routeId = rid}
    else pure ()
