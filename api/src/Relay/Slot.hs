module Relay.Slot
  ( utcTimeToSlot
  , slotToPosixMs
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

-- | Convert a UTCTime to an L1 slot number for the given Cardano network.
--
-- Cardano slot calculation: slot = shelleyStartSlot + floor((utcTime - shelleyStartTime) / slotLength)
--
-- Shelley era has 1-second slots on all networks. The Byron era had 20-second slots
-- so we need the Shelley transition point as our reference.
utcTimeToSlot :: Text -> UTCTime -> Maybe Int64
utcTimeToSlot network utcTime = do
  (shelleyStart, shelleySlot) <- shelleyGenesis network
  let elapsed = utcTime `diffUTCTime` shelleyStart
      slotsSinceShelley = floor elapsed :: Int64
  pure (shelleySlot + slotsSinceShelley)

-- | Convert an L1 slot number to a POSIX-time-in-milliseconds for the given
-- network. Plutus script contexts represent @tx.validity_range@ as a
-- @POSIXTime@ in milliseconds, so any field a script compares against the
-- validity range - like the HTLC validator's @datum.timeout@ - must be in
-- the same unit.
slotToPosixMs :: Text -> Int64 -> Maybe Int64
slotToPosixMs network slot = do
  (shelleyStart, shelleySlot) <- shelleyGenesis network
  let shelleyStartMs = floor (utcTimeToPOSIXSeconds shelleyStart) * 1000 :: Int64
      slotsSinceShelley = slot - shelleySlot
  pure (shelleyStartMs + slotsSinceShelley * 1000)

-- | Shelley era genesis parameters per network.
-- Returns (shelleyStartTime, shelleyStartSlot).
shelleyGenesis :: Text -> Maybe (UTCTime, Int64)
shelleyGenesis "Mainnet" =
  -- Shelley started at slot 4492800, epoch 208, 2020-07-29T21:44:51Z
  Just (posixSecondsToUTCTime 1596059091, 4492800)
shelleyGenesis "Preprod" =
  -- Preprod Shelley at slot 86400, 2022-06-01T00:00:00Z
  Just (posixSecondsToUTCTime 1654041600, 86400)
shelleyGenesis "Preview" =
  -- Preview is all-Shelley from genesis, slot 0, 2022-11-01T00:00:00Z
  Just (posixSecondsToUTCTime 1667260800, 0)
shelleyGenesis _ = Nothing
