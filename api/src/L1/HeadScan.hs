-- | Scan L1 directly for open Hydra head UTxOs via local cardano-nodes.
--
-- For an /Open/ head, a single UTxO at the head validator address carries
-- everything the registry needs, none of which requires datum decoding:
--
--   * the state token's minting policy /is/ the 'headId'
--   * the other token names under that policy are the participants'
--     28-byte key hashes (the OnChainIds that routing runs on)
--   * the UTxO's lovelace is the head's locked value (TVL)
--
-- This sidesteps the hydra-chain-observer limitation of only fully parsing
-- Init transactions of its own protocol version: the token structure is
-- stable across all published versions; only the validator address differs
-- (one entry per version in 'headValidatorHashes').
module L1.HeadScan where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Aeson (toJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Db qualified
import Db.Schema (HeadParticipant (..))
import GHC.IsList (toList)
import Hasql.Pool (Pool)
import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient (QueryPoint (..), cardanoModeParams, queryUTxO)
import Hydra.Htlc (scriptAddressFromHash)
import Logging (Logger, logInfo, logWarn)

-- | One open head as found on chain.
data HeadScanResult = HeadScanResult
  { scanHeadId :: Text
  , scanParticipants :: [Text]
  -- ^ Sorted 28-byte key hashes (hex) from the participation token names.
  , scanLovelace :: Int64
  }
  deriving stock (Eq, Show)

-- | Head validator script hashes of every published hydra version, mirroring
-- hydra-chain-observer's @script-hashes.json@ registry. Data only — no
-- scripts, no datum knowledge.
headValidatorHashes :: [(Text, Text)]
headValidatorHashes =
  [ ("0.13.0", "e35bdf32cd3806596150c1cbab6ab5456bd957b36019ed2746bf481d")
  , ("0.14.0", "e89b0c4a6155bac2434d1e500bd49c155b2b56744ccf5a0efa72a82e")
  , ("0.15.0", "7a36661f5c15e9f1783aeaab890812c59b7286cbbc6de762d3110772")
  , ("0.16.0", "86bff95ba20e9d1d1b34899a56d86bbacc9fed999260b27dcc92d128")
  , ("0.18.0", "bd9fad235c871fb7f837c767593018a84be3083ff80f9dab5f1c55f9")
  , ("0.19.0", "2ee477c60839936be49a50030690865b5bed4db8cd2f05bf255ac680")
  , ("0.20.0", "0e35115a2c7c13c68ecd8d74e4987c04d4539e337643be20bb3274bd")
  , ("0.22.0", "be6ebc744208c660bf0fdc1cfbb5157477cd305de5b1777e575cbb4c")
  , ("1.0.0", "a1442faf26d4ec409e2f62a685c1d4893f8d6bcbaf7bcb59d6fa1340")
  , ("1.3.0", "5788da8969b01bb1d9fd7b78b0dcd988ef2b1d4519e0deae656cef53")
  , ("2.0.0", "21fa6ee40ea957042a3dee8bae69b15c7ed88102be7ecb056d8911b2")
  , ("2.1.0", "4e48afcd87e873c618ae00ee090d52e0451f3d25f1b7d3e397c30b3e")
  , ("2.2.0", "fd75e24c9ea915ce8e48d3ff1d0c54ad09cc01191c24416ad7dba4a3")
  , ("2.3.0", "2b91a7e666575a2465b8c7f6a7f960d5870cf13694a67f3215e014c5")
  ]

-- | Hydra state token asset names across protocol generations.
stAssetNameV1, stAssetNameV2 :: AssetName
stAssetNameV1 = UnsafeAssetName "HydraHeadV1"
stAssetNameV2 = UnsafeAssetName "HydraHeadV2"

isStAssetName :: AssetName -> Bool
isStAssetName an = an == stAssetNameV1 || an == stAssetNameV2

-- | Extract head data from outputs sitting at head validator addresses.
-- An output qualifies when exactly one policy in its value carries a Hydra
-- state token; committed foreign native assets are ignored.
extractHeads :: [TxOut CtxUTxO] -> [HeadScanResult]
extractHeads = mapMaybe extractHead

extractHead :: TxOut CtxUTxO -> Maybe HeadScanResult
extractHead out = do
  let val = txOutValue out
      assets = [(pid, an, q) | (AssetId pid an, q) <- toList val]
  headPid <- case [pid | (pid, an, q) <- assets, isStAssetName an, q == 1] of
    [pid] -> Just pid
    _ -> Nothing
  let participants =
        sort
          [ TE.decodeUtf8 (Base16.encode nameBytes)
          | (pid, an, q) <- assets
          , pid == headPid
          , q == 1
          , not (isStAssetName an)
          , let nameBytes = serialiseToRawBytes an
          , BS.length nameBytes == 28
          ]
      Coin lovelace = selectLovelace val
  pure
    HeadScanResult
      { scanHeadId = serialiseToRawBytesHexText headPid
      , scanParticipants = participants
      , scanLovelace = fromIntegral lovelace
      }

-- | Resolve a registry network name to a cardano NetworkId.
networkIdFor :: Text -> Maybe NetworkId
networkIdFor = \case
  "Mainnet" -> Just Mainnet
  "Preprod" -> Just (Testnet (NetworkMagic 1))
  "Preview" -> Just (Testnet (NetworkMagic 2))
  _ -> Nothing

-- | Query all head validator addresses on one network via a local node
-- socket. Failures are logged and yield no results — the caller's loop
-- must stay alive regardless of node availability.
scanNetwork :: Logger -> Text -> FilePath -> IO [HeadScanResult]
scanNetwork logger network socketPath =
  case networkIdFor network of
    Nothing -> do
      logWarn logger "L1 scan: unknown network" [("network", toJSON network)]
      pure []
    Just networkId -> do
      let addrTexts = [a | (_v, h) <- headValidatorHashes, Right a <- [scriptAddressFromHash h network]]
          addrs = mapMaybe (deserialiseAddress AsShelleyAddress) addrTexts
          connectInfo = LocalNodeConnectInfo cardanoModeParams networkId (File socketPath)
      result <- try @SomeException $ queryUTxO connectInfo QueryTip addrs
      case result of
        Left err -> do
          logWarn logger "L1 scan failed" [("network", toJSON network), ("error", toJSON (show err))]
          pure []
        Right utxo -> do
          let heads = extractHeads (map snd (toList utxo))
          logInfo logger "L1 scan complete" [("network", toJSON network), ("heads", toJSON (length heads))]
          pure heads

-- | Persist scan results: participants (as OnChainIds, matching the
-- Greetings-mirror convention in the indexer) and per-head TVL. Participant
-- rows are only rewritten when the on-chain set differs from what's stored,
-- so richer data from other sources (explorer members with vkeys) is not
-- clobbered by an equivalent scan.
applyScanResults :: Logger -> Pool -> [HeadScanResult] -> IO ()
applyScanResults logger pool results = do
  mapM_ applyOne results
  unless (null results) $
    logInfo logger "L1 scan applied" [("heads", toJSON (length results))]
 where
  applyOne r = do
    existing <- Db.getParticipantsForHead pool r.scanHeadId
    let existingAddrs = sort [a | HeadParticipant{participantAddress = a} <- existing]
    when (not (null r.scanParticipants) && existingAddrs /= r.scanParticipants) $
      Db.replaceHeadParticipants
        pool
        r.scanHeadId
        [(p, Nothing, Just p, 0, Nothing) | p <- r.scanParticipants]
    Db.updateExplorerHeadTvl pool r.scanHeadId r.scanLovelace
