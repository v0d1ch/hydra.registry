module Relay.HtlcWatcher
  ( processUtxoSnapshot
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Db qualified
import Db.Schema (PaymentRoute (..), RouteHop (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraUtxoEntry (..))
import Logging

-- | After each snapshot, detect HTLC lock and claim events by comparing
-- the new UTxO set against active route hops for this head.
--
-- Lock: a new UTxO at the HTLC script address with an inline datum
--       whose payment hash matches a pending hop → mark hop as 'locked'.
--
-- Claim: a previously locked hop's HTLC tx hash is no longer in the
--        UTxO set (spent) → mark hop as 'claimed'.
processUtxoSnapshot :: Logger -> Pool -> Text -> Text -> [HydraUtxoEntry] -> IO ()
processUtxoSnapshot logger pool headId htlcScriptHash utxos = do
  -- Get all active (pending or locked) hops for this head
  activeHops <- Db.getActiveHopsByHead pool headId

  -- Detect locks: UTxOs at HTLC script address with matching payment hash
  let htlcUtxos = filter (isHtlcUtxo htlcScriptHash) utxos
  mapM_ (detectLock logger pool headId activeHops) htlcUtxos

  -- Detect claims: locked hops whose HTLC tx is no longer in the UTxO set
  let utxoTxHashes = map (\u -> u.txHash) utxos
  mapM_ (detectClaim logger pool utxoTxHashes) activeHops

-- | Check if a UTxO sits at the HTLC script address.
-- We identify HTLC UTxOs by checking if the address contains the script hash,
-- since script addresses embed the script hash.
isHtlcUtxo :: Text -> HydraUtxoEntry -> Bool
isHtlcUtxo scriptHash entry =
  scriptHash `T.isInfixOf` entry.address
    && hasInlineDatum entry
 where
  hasInlineDatum e = case e.inlineDatum of
    Just _ -> True
    Nothing -> False

-- | Try to match an HTLC UTxO to a pending hop and mark it as locked.
detectLock :: Logger -> Pool -> Text -> [RouteHop Identity] -> HydraUtxoEntry -> IO ()
detectLock logger pool headId activeHops entry = do
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
      mapM_
        ( \h -> do
            logInfo logger "HTLC lock detected" [("hopId", Aeson.toJSON h.hopId), ("headId", Aeson.toJSON headId), ("txHash", Aeson.toJSON entry.txHash)]
            Db.updateHopLocked pool h.hopId entry.txHash now
        )
        matchingHops

-- | Check if a locked hop's HTLC UTxO has been spent (claimed).
detectClaim :: Logger -> Pool -> [Text] -> RouteHop Identity -> IO ()
detectClaim logger pool utxoTxHashes hop = do
  case hop.hopHtlcTxHash of
    Nothing -> pure ()
    Just lockedTxHash ->
      -- If the locked tx hash is no longer in the UTxO set, it was spent (claimed)
      if lockedTxHash `notElem` utxoTxHashes && hop.hopHtlcStatus == "locked"
        then do
          now <- getCurrentTime
          logInfo logger "HTLC claim detected" [("hopId", Aeson.toJSON hop.hopId), ("txHash", Aeson.toJSON lockedTxHash)]
          Db.updateHopClaimed pool hop.hopId now
          -- Check if all hops in this route are now claimed → complete the payment
          checkRouteCompletion logger pool hop.hopRouteId
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
checkRouteCompletion :: Logger -> Pool -> Text -> IO ()
checkRouteCompletion logger pool routeId = do
  hops <- Db.getRouteHops pool routeId
  let allClaimed = all (\h -> h.hopHtlcStatus == "claimed") hops
  if allClaimed && not (null hops)
    then do
      logInfo logger "All hops claimed — payment complete" [("routeId", Aeson.toJSON routeId)]
      Db.updateRouteStatus pool routeId "completed"
      -- Mark the invoice as paid
      mRoute <- Db.getPaymentRoute pool routeId
      case mRoute of
        Just route -> Db.updateInvoiceStatus pool route.routeInvoiceId "paid"
        Nothing -> pure ()
    else pure ()
