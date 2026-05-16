module Tx.Builder
  ( BuildResult (..)
  , LockArgs (..)
  , ClaimArgs (..)
  , RefundArgs (..)
  , PublishRefArgs (..)
  , buildLockTx
  , buildClaimTx
  , buildRefundTx
  , buildPublishRefTx
  , extractPkhFromAddress
  , -- exposed for unit tests
    lockTxArgs
  , claimTxArgs
  , refundTxArgs
  , publishRefTxArgs
  , htlcExecUnits
  )
where

import Codec.Binary.Bech32 qualified as Bech32
import Control.Exception (IOException, bracket, try)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Result of a successful tx build.
--
-- Serialises as a flat cardano-cli text-envelope JSON
-- (@type@, @description@, @cborHex@) with @txId@ added alongside —
-- cardano-cli ignores unknown keys, so the file can be passed directly
-- to @cardano-cli conway transaction sign --tx-file@.
data BuildResult = BuildResult
  { cborHex :: Text
  , txId :: Text
  , envelope :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON)

instance Aeson.ToJSON BuildResult where
  toJSON BuildResult{txId, envelope} =
    case envelope of
      Aeson.Object km -> Aeson.Object (KM.insert (Key.fromString "txId") (Aeson.String txId) km)
      other -> other

-- ─── argument records ──────────────────────────────────────────────────

-- | Inputs required to build an HTLC lock tx — the locker spends
-- one wallet UTxO into a script-address output that carries the
-- HTLC datum (and, when no shared ref-script is published, the
-- inline ref script).
data LockArgs = LockArgs
  { -- | @"<txhash>#<ix>"@ of the wallet UTxO funding the lock.
    inputUtxo :: Text
  , -- | Lovelace value of the input (used to compute change).
    inputLovelace :: Int64
  , -- | Bech32 address that owns the input UTxO; change goes here.
    walletAddress :: Text
  , -- | Bech32 HTLC script address (where the lock output lives).
    scriptAddress :: Text
  , -- | Plutus Data CBOR for the HTLC datum, hex-encoded.
    datumCborHex :: Text
  , -- | When @Just@: the head has a shared HTLC ref-script UTxO and
    -- the lock output omits the inline script. When @Nothing@: a
    -- Plutus envelope file is written and inlined as the output's
    -- @reference_script@.
    sharedRefUtxo :: Maybe Text
  , -- | Lovelace to lock at the script address.
    lockAmount :: Int64
  , -- | @--invalid-hereafter@ slot.
    validityUpperSlot :: Int64
  , -- | Pkh of the locker (sender of this hop). Set as
    -- @--required-signer-hash@.
    requiredSignerPkhHex :: Text
  , -- | Fee in lovelace.
    feeLovelace :: Int64
  , -- | Conway protocol-parameters JSON (the response body from
    -- the head's @GET /protocol-parameters@). The builder writes
    -- it to its own temp dir before invoking @cardano-cli@.
    protocolParamsJson :: Text
  , -- | Plutus V3 envelope JSON (with @cborHex@) for the HTLC
    -- validator. Required only when 'sharedRefUtxo' is 'Nothing'.
    plutusEnvelope :: Maybe Value
  }

-- | Inputs required to build an HTLC claim tx — the claimer spends
-- the locked HTLC UTxO using a 'Claim(preimage)' redeemer.
data ClaimArgs = ClaimArgs
  { -- | @"<txhash>"@ of the lock tx; we always use index 0.
    htlcInputTxHash :: Text
  , htlcInputIndex :: Int
  , -- | The head's published ref-script UTxO
    -- (@"<txhash>#<ix>"@). Claims always use --spending-tx-in-reference;
    -- the lock output's own inline ref script can't satisfy it because
    -- the same UTxO can't be both input and reference input.
    refScriptUtxo :: Text
  , -- | Plutus Data CBOR for @Claim(preimage)@.
    redeemerCborHex :: Text
  , -- | Pure-ADA collateral input — @"<txhash>#<ix>"@.
    collateralUtxo :: Text
  , -- | Lovelace value of the collateral input.
    collateralLovelace :: Int64
  , -- | @--tx-total-collateral@ — pledged amount (≤ collateralLovelace).
    totalCollateralLovelace :: Int64
  , -- | Where the claim output goes (typically the claimer's
    -- wallet address inside the head). Also receives the
    -- return-collateral if one is needed.
    walletAddress :: Text
  , -- | Lovelace value of the locked HTLC UTxO; the claim output
    -- gets @htlcOutputLovelace - feeLovelace@.
    htlcOutputLovelace :: Int64
  , validityUpperSlot :: Int64
  , -- | Claimer's pkh.
    requiredSignerPkhHex :: Text
  , feeLovelace :: Int64
  , protocolParamsJson :: Text
  }

-- | Inputs for an HTLC refund tx (mirror of claim, but with the
-- @Refund@ redeemer and @--invalid-before@).
data RefundArgs = RefundArgs
  { htlcInputTxHash :: Text
  , htlcInputIndex :: Int
  , refScriptUtxo :: Text
  , redeemerCborHex :: Text
  , collateralUtxo :: Text
  , collateralLovelace :: Int64
  , totalCollateralLovelace :: Int64
  , walletAddress :: Text
  , htlcOutputLovelace :: Int64
  , validityLowerSlot :: Int64
  , requiredSignerPkhHex :: Text
  , feeLovelace :: Int64
  , protocolParamsJson :: Text
  }

-- | Inputs for the operator helper tx that publishes the HTLC
-- validator as an inline reference script in a small UTxO at the
-- operator's wallet address. After submission, the operator
-- registers the resulting @"<txhash>#<ix>"@ via
-- @POST /heads/{id}/ref-script@ so future locks can drop their
-- min-ada and future claims can use --spending-tx-in-reference
-- against this UTxO.
data PublishRefArgs = PublishRefArgs
  { inputUtxo :: Text
  , inputLovelace :: Int64
  , walletAddress :: Text
  , -- | Lovelace placed on the ref-script output. ~6 ADA covers
    -- min-ada for the script + a small cushion.
    refOutputLovelace :: Int64
  , feeLovelace :: Int64
  , protocolParamsJson :: Text
  , -- | Plutus V3 envelope JSON for the HTLC validator.
    plutusEnvelope :: Value
  }

-- ─── exec units ───────────────────────────────────────────────────────

-- | Plutus execution-units budget @(steps, mem)@ for a single HTLC
-- claim or refund. Measured against the actual validator on
-- Preview during the manual e2e (2026-04-30) and rounded up to give
-- headroom against future param shifts. Sized once here, not
-- estimated per-tx — our validator is small and shape-stable.
htlcExecUnits :: (Integer, Integer)
htlcExecUnits = (10_000_000_000, 4_000_000)

-- | Extract the 28-byte payment key hash from a bech32 Cardano address.
-- Works for enterprise addresses (addr1/addr_test1) where byte[0] is the
-- header and bytes[1..28] are the payment pkh. Does not validate the
-- header byte — callers should ensure the address is an enterprise address.
extractPkhFromAddress :: Text -> Either Text Text
extractPkhFromAddress addr =
  case Bech32.decode addr of
    Left _ -> Left "invalid bech32 address"
    Right (_, dp) -> case Bech32.dataPartToBytes dp of
      Nothing -> Left "invalid bech32 data part"
      Just bytes
        | BS.length bytes < 29 -> Left "address too short"
        | otherwise -> Right $ T.decodeUtf8 $ Base16.encode $ BS.take 28 $ BS.drop 1 bytes

-- ─── builders ──────────────────────────────────────────────────────────

-- | Build a lock tx. May fail if @cardano-cli@ exits non-zero, the
-- output envelope can't be parsed, or 'sharedRefUtxo' is 'Nothing'
-- but no 'plutusEnvelope' is supplied.
buildLockTx :: LockArgs -> IO (Either Text BuildResult)
buildLockTx args =
  case (args.sharedRefUtxo, args.plutusEnvelope) of
    (Nothing, Nothing) ->
      pure (Left "lock: no shared ref-script UTxO and no plutus envelope was supplied")
    _ ->
      withTempDir $ \dir -> do
        let datumPath = dir </> "datum.cbor"
            envPath = dir </> "htlc.plutus.json"
            ppPath = dir </> "pp.json"
            outPath = dir </> "tx.raw"
        writeBinFromHex datumPath args.datumCborHex
        BS.writeFile ppPath (T.encodeUtf8 args.protocolParamsJson)
        case args.plutusEnvelope of
          Just env -> BSL.writeFile envPath (Aeson.encode env)
          Nothing -> pure ()
        runBuildAndPackage outPath (lockTxArgs args datumPath envPath ppPath outPath)

-- | Build a claim tx.
buildClaimTx :: ClaimArgs -> IO (Either Text BuildResult)
buildClaimTx args = withTempDir $ \dir -> do
  let redeemerPath = dir </> "redeemer.cbor"
      ppPath = dir </> "pp.json"
      outPath = dir </> "tx.raw"
  writeBinFromHex redeemerPath args.redeemerCborHex
  BS.writeFile ppPath (T.encodeUtf8 args.protocolParamsJson)
  runBuildAndPackage outPath (claimTxArgs args redeemerPath ppPath outPath)

-- | Build a refund tx.
buildRefundTx :: RefundArgs -> IO (Either Text BuildResult)
buildRefundTx args = withTempDir $ \dir -> do
  let redeemerPath = dir </> "redeemer.cbor"
      ppPath = dir </> "pp.json"
      outPath = dir </> "tx.raw"
  writeBinFromHex redeemerPath args.redeemerCborHex
  BS.writeFile ppPath (T.encodeUtf8 args.protocolParamsJson)
  runBuildAndPackage outPath (refundTxArgs args redeemerPath ppPath outPath)

-- | Build a publish-ref-script tx.
buildPublishRefTx :: PublishRefArgs -> IO (Either Text BuildResult)
buildPublishRefTx args = withTempDir $ \dir -> do
  let envPath = dir </> "htlc.plutus.json"
      ppPath = dir </> "pp.json"
      outPath = dir </> "tx.raw"
  BSL.writeFile envPath (Aeson.encode args.plutusEnvelope)
  BS.writeFile ppPath (T.encodeUtf8 args.protocolParamsJson)
  runBuildAndPackage outPath (publishRefTxArgs args envPath ppPath outPath)

-- ─── pure command-line generation (exposed for tests) ─────────────────

-- | Conway @transaction build-raw@ argv for a lock tx, given the
-- on-disk paths for the datum binary, the (optional) plutus
-- envelope JSON, the protocol-parameters JSON, and the output tx
-- file.
lockTxArgs :: LockArgs -> FilePath -> FilePath -> FilePath -> FilePath -> [String]
lockTxArgs args datumPath envPath ppPath outPath =
  let lockOutFlags =
        [ "--tx-out"
        , T.unpack args.scriptAddress <> "+" <> show args.lockAmount
        , "--tx-out-inline-datum-cbor-file"
        , datumPath
        ]
          <> case args.sharedRefUtxo of
            -- With a shared ref UTxO, the lock output omits the
            -- inline script. (Future locks can't reference *this*
            -- output's script anyway — they'd hit
            -- BabbageNonDisjointRefInputs — so the inline-on-output
            -- form is only useful in the no-shared-ref case.)
            Just _ -> []
            Nothing -> ["--tx-out-reference-script-file", envPath]
      change = args.inputLovelace - args.lockAmount - args.feeLovelace
   in [ "conway"
      , "transaction"
      , "build-raw"
      , "--tx-in"
      , T.unpack args.inputUtxo
      ]
        <> lockOutFlags
        <> [ "--tx-out"
           , T.unpack args.walletAddress <> "+" <> show change
           , "--invalid-hereafter"
           , show args.validityUpperSlot
           , "--required-signer-hash"
           , T.unpack args.requiredSignerPkhHex
           , "--fee"
           , show args.feeLovelace
           , "--protocol-params-file"
           , ppPath
           , "--out-file"
           , outPath
           ]

-- | Conway @transaction build-raw@ argv for a claim tx.
claimTxArgs :: ClaimArgs -> FilePath -> FilePath -> FilePath -> [String]
claimTxArgs args redeemerPath ppPath outPath =
  let (steps, mem) = htlcExecUnits
      claimOut = args.htlcOutputLovelace - args.feeLovelace
      returnCollateral = args.collateralLovelace - args.totalCollateralLovelace
   in [ "conway"
      , "transaction"
      , "build-raw"
      , "--tx-in"
      , T.unpack args.htlcInputTxHash <> "#" <> show args.htlcInputIndex
      , "--spending-tx-in-reference"
      , T.unpack args.refScriptUtxo
      , "--spending-plutus-script-v3"
      , "--spending-reference-tx-in-inline-datum-present"
      , "--spending-reference-tx-in-redeemer-cbor-file"
      , redeemerPath
      , "--spending-reference-tx-in-execution-units"
      , "(" <> show steps <> "," <> show mem <> ")"
      , "--tx-in-collateral"
      , T.unpack args.collateralUtxo
      , "--tx-out-return-collateral"
      , T.unpack args.walletAddress <> "+" <> show returnCollateral
      , "--tx-total-collateral"
      , show args.totalCollateralLovelace
      , "--tx-out"
      , T.unpack args.walletAddress <> "+" <> show claimOut
      , "--invalid-hereafter"
      , show args.validityUpperSlot
      , "--required-signer-hash"
      , T.unpack args.requiredSignerPkhHex
      , "--fee"
      , show args.feeLovelace
      , "--protocol-params-file"
      , ppPath
      , "--out-file"
      , outPath
      ]

-- | Conway @transaction build-raw@ argv for a refund tx — mirror of
-- claim, but with @--invalid-before@ (refund must happen *after*
-- the timeout).
refundTxArgs :: RefundArgs -> FilePath -> FilePath -> FilePath -> [String]
refundTxArgs args redeemerPath ppPath outPath =
  let (steps, mem) = htlcExecUnits
      refundOut = args.htlcOutputLovelace - args.feeLovelace
      returnCollateral = args.collateralLovelace - args.totalCollateralLovelace
   in [ "conway"
      , "transaction"
      , "build-raw"
      , "--tx-in"
      , T.unpack args.htlcInputTxHash <> "#" <> show args.htlcInputIndex
      , "--spending-tx-in-reference"
      , T.unpack args.refScriptUtxo
      , "--spending-plutus-script-v3"
      , "--spending-reference-tx-in-inline-datum-present"
      , "--spending-reference-tx-in-redeemer-cbor-file"
      , redeemerPath
      , "--spending-reference-tx-in-execution-units"
      , "(" <> show steps <> "," <> show mem <> ")"
      , "--tx-in-collateral"
      , T.unpack args.collateralUtxo
      , "--tx-out-return-collateral"
      , T.unpack args.walletAddress <> "+" <> show returnCollateral
      , "--tx-total-collateral"
      , show args.totalCollateralLovelace
      , "--tx-out"
      , T.unpack args.walletAddress <> "+" <> show refundOut
      , "--invalid-before"
      , show args.validityLowerSlot
      , "--required-signer-hash"
      , T.unpack args.requiredSignerPkhHex
      , "--fee"
      , show args.feeLovelace
      , "--protocol-params-file"
      , ppPath
      , "--out-file"
      , outPath
      ]

-- | Conway @transaction build-raw@ argv for a publish-ref-script tx.
publishRefTxArgs :: PublishRefArgs -> FilePath -> FilePath -> FilePath -> [String]
publishRefTxArgs args envPath ppPath outPath =
  let change = args.inputLovelace - args.refOutputLovelace - args.feeLovelace
   in [ "conway"
      , "transaction"
      , "build-raw"
      , "--tx-in"
      , T.unpack args.inputUtxo
      , "--tx-out"
      , T.unpack args.walletAddress <> "+" <> show args.refOutputLovelace
      , "--tx-out-reference-script-file"
      , envPath
      , "--tx-out"
      , T.unpack args.walletAddress <> "+" <> show change
      , "--fee"
      , show args.feeLovelace
      , "--protocol-params-file"
      , ppPath
      , "--out-file"
      , outPath
      ]

-- ─── shell-out plumbing ──────────────────────────────────────────────

runBuildAndPackage :: FilePath -> [String] -> IO (Either Text BuildResult)
runBuildAndPackage outPath cliArgs = do
  buildE <- runCardanoCli cliArgs
  case buildE of
    Left err -> pure (Left err)
    Right () -> do
      raw <- BSL.readFile outPath
      case Aeson.eitherDecode raw of
        Left e -> pure (Left ("could not parse build-raw output: " <> T.pack e))
        Right env -> do
          tidE <- runCardanoCliCapture ["conway", "transaction", "txid", "--tx-file", outPath]
          case tidE of
            Left err -> pure (Left err)
            Right tidStdout ->
              -- @cardano-cli transaction txid@ may emit either bare
              -- hex or a JSON @{"txhash":"…"}@; handle both.
              let trimmed = T.strip tidStdout
                  txid = case Aeson.eitherDecodeStrict (T.encodeUtf8 trimmed) of
                    Right (Aeson.Object o)
                      | Just (Aeson.String s) <- KM.lookup (Key.fromString "txhash") o -> s
                    _ -> trimmed
                  cbor = case env of
                    Aeson.Object o
                      | Just (Aeson.String s) <- KM.lookup (Key.fromString "cborHex") o -> s
                    _ -> ""
               in pure $
                    Right
                      BuildResult
                        { cborHex = cbor
                        , txId = txid
                        , envelope = env
                        }

-- | Run @cardano-cli ARGS@ with no captured stdout — failure goes
-- through stderr.
runCardanoCli :: [String] -> IO (Either Text ())
runCardanoCli cliArgs = do
  res <- try @IOException $ readProcessWithExitCode "cardano-cli" cliArgs ""
  pure $ case res of
    Left e -> Left ("cardano-cli not runnable: " <> T.pack (show e))
    Right (ExitSuccess, _, _) -> Right ()
    Right (ExitFailure n, out, err) ->
      Left $
        "cardano-cli exit "
          <> T.pack (show n)
          <> ": "
          <> T.pack (lastNonEmpty out err)

runCardanoCliCapture :: [String] -> IO (Either Text Text)
runCardanoCliCapture cliArgs = do
  res <- try @IOException $ readProcessWithExitCode "cardano-cli" cliArgs ""
  pure $ case res of
    Left e -> Left ("cardano-cli not runnable: " <> T.pack (show e))
    Right (ExitSuccess, out, _) -> Right (T.pack out)
    Right (ExitFailure n, out, err) ->
      Left $
        "cardano-cli exit "
          <> T.pack (show n)
          <> ": "
          <> T.pack (lastNonEmpty out err)

lastNonEmpty :: String -> String -> String
lastNonEmpty a b
  | not (null b) = b
  | otherwise = a

-- | Hex → bytes, written to a file. Used for datum and redeemer
-- CBOR which @cardano-cli@ wants in raw binary form.
writeBinFromHex :: FilePath -> Text -> IO ()
writeBinFromHex path hex =
  case Base16.decode (T.encodeUtf8 hex) of
    Left _ -> BS.writeFile path BS.empty
    Right b -> BS.writeFile path b

-- | A bracketed temp directory under @/tmp@. Unique name per call;
-- recursively removed on exit even if the action raises.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir =
  bracket alloc removeDirectoryRecursive
  where
    alloc = do
      uuid <- UUID.toString <$> UUID.nextRandom
      let dir = "/tmp/hydra-registry-tx-" <> uuid
      createDirectoryIfMissing True dir
      pure dir

