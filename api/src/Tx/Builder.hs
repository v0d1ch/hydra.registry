-- | Native construction of the four HTLC transaction shapes, using
-- @hydra-cardano-api@ (Conway era) instead of shelling out to
-- @cardano-cli conway transaction build-raw@.
--
-- The builders are pure: given fully-resolved arguments they either
-- return the unsigned transaction (CBOR + txId + text envelope) or a
-- descriptive error. Fees and execution units are explicit, exactly as
-- they were with @build-raw@ — nothing is balanced or estimated here.
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
  , htlcExecUnits
  )
where

import Cardano.Api qualified as CApi
import Codec.Binary.Bech32 qualified as Bech32
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Function ((&))
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Hydra.Cardano.Api
import Text.Read (readMaybe)

-- | Result of a successful tx build.
--
-- Serialises as a flat cardano-cli text-envelope JSON
-- (@type@, @description@, @cborHex@) with @txId@ added alongside —
-- cardano-cli ignores unknown keys, so the file can be passed directly
-- to @cardano-cli conway transaction sign --tx-file@.
data BuildResult = BuildResult
  { cborHex :: Text
  , txId :: Text
  , envelope :: Aeson.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON)

instance Aeson.ToJSON BuildResult where
  toJSON BuildResult{txId = tid, envelope} =
    case envelope of
      Aeson.Object km -> Aeson.Object (KM.insert (Key.fromString "txId") (Aeson.String tid) km)
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
    -- the lock output omits the inline script. When @Nothing@: the
    -- 'plutusEnvelope' is attached as the output's reference script.
    sharedRefUtxo :: Maybe Text
  , -- | Lovelace to lock at the script address.
    lockAmount :: Int64
  , -- | Upper validity bound (@--invalid-hereafter@ equivalent).
    validityUpperSlot :: Int64
  , -- | Pkh of the locker (sender of this hop), listed as a
    -- required signer.
    requiredSignerPkhHex :: Text
  , -- | Fee in lovelace.
    feeLovelace :: Int64
  , -- | Conway protocol-parameters JSON (the response body from
    -- the head's @GET /protocol-parameters@). Unused for the lock
    -- tx (nothing is executed), kept for interface stability.
    protocolParamsJson :: Text
  , -- | Plutus V3 envelope JSON (with @cborHex@) for the HTLC
    -- validator. Required only when 'sharedRefUtxo' is 'Nothing'.
    plutusEnvelope :: Maybe Aeson.Value
  }

-- | Inputs required to build an HTLC claim tx — the claimer spends
-- the locked HTLC UTxO using a 'Claim(preimage)' redeemer.
data ClaimArgs = ClaimArgs
  { -- | @"<txhash>"@ of the lock tx.
    htlcInputTxHash :: Text
  , htlcInputIndex :: Int
  , -- | The head's published ref-script UTxO
    -- (@"<txhash>#<ix>"@). Claims always spend via the reference
    -- script; the lock output's own inline ref script can't satisfy
    -- it because the same UTxO can't be both input and reference
    -- input.
    refScriptUtxo :: Text
  , -- | Plutus Data CBOR for @Claim(preimage)@, hex-encoded.
    redeemerCborHex :: Text
  , -- | Pure-ADA collateral input — @"<txhash>#<ix>"@.
    collateralUtxo :: Text
  , -- | Lovelace value of the collateral input.
    collateralLovelace :: Int64
  , -- | Pledged collateral (≤ collateralLovelace).
    totalCollateralLovelace :: Int64
  , -- | Where the claim output goes (typically the claimer's
    -- wallet address inside the head). Also receives the
    -- return-collateral.
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
-- @Refund@ redeemer and a lower validity bound).
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
-- min-ada and future claims can spend via this UTxO.
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
    plutusEnvelope :: Aeson.Value
  }

-- ─── exec units ───────────────────────────────────────────────────────

-- | Plutus execution-units budget @(steps, mem)@ for a single HTLC
-- claim or refund. Measured against the actual validator on
-- Preview during the manual e2e (2026-04-30) and rounded up to give
-- headroom against future param shifts. Sized once here, not
-- estimated per-tx — our validator is small and shape-stable.
htlcExecUnits :: (Integer, Integer)
htlcExecUnits = (10_000_000_000, 4_000_000)

htlcExecutionUnits :: ExecutionUnits
htlcExecutionUnits =
  let (steps, mem) = htlcExecUnits
   in ExecutionUnits
        { executionSteps = fromInteger steps
        , executionMemory = fromInteger mem
        }

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
        | otherwise -> Right $ TE.decodeUtf8 $ Base16.encode $ BS.take 28 $ BS.drop 1 bytes

-- ─── builders ──────────────────────────────────────────────────────────

-- | Build a lock tx: wallet input → script output (inline datum,
-- optional inline ref script) + change output.
buildLockTx :: LockArgs -> Either Text BuildResult
buildLockTx args = do
  refScript <- case (args.sharedRefUtxo, args.plutusEnvelope) of
    (Just _, _) -> pure ReferenceScriptNone
    (Nothing, Just env) -> mkScriptRef <$> parsePlutusEnvelope env
    (Nothing, Nothing) ->
      Left "lock: no shared ref-script UTxO and no plutus envelope was supplied"
  txIn <- parseUtxoRef args.inputUtxo
  scriptAddr <- parseAddress args.scriptAddress
  walletAddr <- parseAddress args.walletAddress
  datum <- parseScriptData "datum" args.datumCborHex
  signer <- parsePkh args.requiredSignerPkhHex
  let change = args.inputLovelace - args.lockAmount - args.feeLovelace
      content =
        defaultTxBodyContent
          & setTxIns [withWitness txIn]
          & setTxOuts
            [ TxOut
                scriptAddr
                (lovelaceToValue (Coin (fromIntegral args.lockAmount)))
                (TxOutDatumInline datum)
                refScript
            , TxOut
                walletAddr
                (lovelaceToValue (Coin (fromIntegral change)))
                TxOutDatumNone
                ReferenceScriptNone
            ]
          & setTxFee (TxFeeExplicit (Coin (fromIntegral args.feeLovelace)))
          & setTxValidityUpperBound (TxValidityUpperBound (SlotNo (fromIntegral args.validityUpperSlot)))
          & setTxExtraKeyWits (TxExtraKeyWitnesses [signer])
  packageTx "lock" content

-- | Build a claim tx: spend the HTLC UTxO via the published
-- reference script with the @Claim(preimage)@ redeemer.
buildClaimTx :: ClaimArgs -> Either Text BuildResult
buildClaimTx args =
  spendHtlcTx
    "claim"
    SpendHtlcArgs
      { inputTxHash = args.htlcInputTxHash
      , inputIndex = args.htlcInputIndex
      , refScriptUtxo = args.refScriptUtxo
      , redeemerCborHex = args.redeemerCborHex
      , collateralUtxo = args.collateralUtxo
      , collateralLovelace = args.collateralLovelace
      , totalCollateralLovelace = args.totalCollateralLovelace
      , walletAddress = args.walletAddress
      , htlcOutputLovelace = args.htlcOutputLovelace
      , requiredSignerPkhHex = args.requiredSignerPkhHex
      , feeLovelace = args.feeLovelace
      , protocolParamsJson = args.protocolParamsJson
      , validityLower = TxValidityNoLowerBound
      , validityUpper = TxValidityUpperBound (SlotNo (fromIntegral args.validityUpperSlot))
      }

-- | Build a refund tx — mirror of claim, but with the @Refund@
-- redeemer and a lower validity bound (refund must happen *after*
-- the timeout).
buildRefundTx :: RefundArgs -> Either Text BuildResult
buildRefundTx args =
  spendHtlcTx
    "refund"
    SpendHtlcArgs
      { inputTxHash = args.htlcInputTxHash
      , inputIndex = args.htlcInputIndex
      , refScriptUtxo = args.refScriptUtxo
      , redeemerCborHex = args.redeemerCborHex
      , collateralUtxo = args.collateralUtxo
      , collateralLovelace = args.collateralLovelace
      , totalCollateralLovelace = args.totalCollateralLovelace
      , walletAddress = args.walletAddress
      , htlcOutputLovelace = args.htlcOutputLovelace
      , requiredSignerPkhHex = args.requiredSignerPkhHex
      , feeLovelace = args.feeLovelace
      , protocolParamsJson = args.protocolParamsJson
      , validityLower = TxValidityLowerBound (SlotNo (fromIntegral args.validityLowerSlot))
      , validityUpper = TxValidityNoUpperBound
      }

-- | Build a publish-ref-script tx: wallet input → small output
-- carrying the validator as an inline reference script + change.
-- A plain key-witnessed tx: no scripts execute.
buildPublishRefTx :: PublishRefArgs -> Either Text BuildResult
buildPublishRefTx args = do
  script <- parsePlutusEnvelope args.plutusEnvelope
  txIn <- parseUtxoRef args.inputUtxo
  walletAddr <- parseAddress args.walletAddress
  let change = args.inputLovelace - args.refOutputLovelace - args.feeLovelace
      content =
        defaultTxBodyContent
          & setTxIns [withWitness txIn]
          & setTxOuts
            [ TxOut
                walletAddr
                (lovelaceToValue (Coin (fromIntegral args.refOutputLovelace)))
                TxOutDatumNone
                (mkScriptRef script)
            , TxOut
                walletAddr
                (lovelaceToValue (Coin (fromIntegral change)))
                TxOutDatumNone
                ReferenceScriptNone
            ]
          & setTxFee (TxFeeExplicit (Coin (fromIntegral args.feeLovelace)))
  packageTx "publish-ref" content

-- ─── shared HTLC spending core ─────────────────────────────────────────

-- | What claim and refund have in common: spend the HTLC UTxO via
-- the reference script with an explicit redeemer, budget, fee and
-- collateral; only the validity interval differs.
data SpendHtlcArgs = SpendHtlcArgs
  { inputTxHash :: Text
  , inputIndex :: Int
  , refScriptUtxo :: Text
  , redeemerCborHex :: Text
  , collateralUtxo :: Text
  , collateralLovelace :: Int64
  , totalCollateralLovelace :: Int64
  , walletAddress :: Text
  , htlcOutputLovelace :: Int64
  , requiredSignerPkhHex :: Text
  , feeLovelace :: Int64
  , protocolParamsJson :: Text
  , validityLower :: TxValidityLowerBound
  , validityUpper :: TxValidityUpperBound
  }

spendHtlcTx :: Text -> SpendHtlcArgs -> Either Text BuildResult
spendHtlcTx ctx args = do
  htlcIn <- parseUtxoRef (args.inputTxHash <> "#" <> T.pack (show args.inputIndex))
  refIn <- parseUtxoRef args.refScriptUtxo
  collateralIn <- parseUtxoRef args.collateralUtxo
  walletAddr <- parseAddress args.walletAddress
  redeemer <- parseScriptData "redeemer" args.redeemerCborHex
  signer <- parsePkh args.requiredSignerPkhHex
  pparams <- parsePParams args.protocolParamsJson
  let witness =
        CApi.ScriptWitness ScriptWitnessForSpending $
          CApi.PlutusScriptWitness
            scriptLanguageInEra
            CApi.PlutusScriptV3
            (PReferenceScript refIn)
            InlineScriptDatum
            redeemer
            htlcExecutionUnits
      payout = args.htlcOutputLovelace - args.feeLovelace
      returnCollateral = args.collateralLovelace - args.totalCollateralLovelace
      content =
        defaultTxBodyContent
          & setTxIns [(htlcIn, BuildTxWith witness)]
          & setTxInsReference (TxInsReference [refIn] (BuildTxWith mempty))
          & setTxInsCollateral (TxInsCollateral [collateralIn])
          & setTxTotalCollateral
            (CApi.TxTotalCollateral babbageBasedEra (Coin (fromIntegral args.totalCollateralLovelace)))
          & setTxReturnCollateral
            ( CApi.TxReturnCollateral babbageBasedEra $
                TxOut
                  walletAddr
                  (lovelaceToValue (Coin (fromIntegral returnCollateral)))
                  TxOutDatumNone
                  ReferenceScriptNone
            )
          & setTxOuts
            [ TxOut
                walletAddr
                (lovelaceToValue (Coin (fromIntegral payout)))
                TxOutDatumNone
                ReferenceScriptNone
            ]
          & setTxFee (TxFeeExplicit (Coin (fromIntegral args.feeLovelace)))
          & setTxValidityLowerBound args.validityLower
          & setTxValidityUpperBound args.validityUpper
          & setTxExtraKeyWits (TxExtraKeyWitnesses [signer])
          & setTxProtocolParams (BuildTxWith (Just (LedgerProtocolParameters pparams)))
  packageTx ctx content

-- ─── body construction and packaging ───────────────────────────────────

-- | Build the body, wrap it in an unsigned tx and serialise the
-- text envelope, CBOR hex, and txId.
packageTx :: Text -> TxBodyContent BuildTx -> Either Text BuildResult
packageTx ctx content = do
  body <-
    first (\e -> ctx <> ": could not build tx body: " <> T.pack (show e)) $
      createAndValidateTransactionBody content
  let tx = makeSignedTransaction [] body
  pure
    BuildResult
      { cborHex = TE.decodeUtf8 (Base16.encode (serialiseToCBOR tx))
      , txId = serialiseToRawBytesHexText (getTxId body)
      , envelope = Aeson.toJSON (serialiseToTextEnvelope Nothing tx)
      }

-- ─── parsers ───────────────────────────────────────────────────────────

-- | Parse a @"<txhash>#<ix>"@ UTxO reference.
parseUtxoRef :: Text -> Either Text TxIn
parseUtxoRef t =
  case T.splitOn "#" t of
    [hashText, ixText] -> do
      tid <-
        first (\e -> "invalid tx id in \"" <> t <> "\": " <> T.pack (show e)) $
          (deserialiseFromRawBytesHex (TE.encodeUtf8 hashText) :: Either RawBytesHexError TxId)
      ix <-
        maybe (Left ("invalid tx index in \"" <> t <> "\"")) Right $
          readMaybe (T.unpack ixText)
      pure (TxIn tid (TxIx ix))
    _ -> Left ("invalid UTxO reference (expected \"<txhash>#<ix>\"): " <> t)

-- | Parse a bech32 payment address into the Conway era.
parseAddress :: Text -> Either Text AddressInEra
parseAddress t =
  maybe (Left ("invalid bech32 address: " <> t)) Right $
    deserialiseAddress (AsAddressInEra AsConwayEra) t

-- | Parse hex-encoded Plutus Data CBOR (datum or redeemer).
parseScriptData :: Text -> Text -> Either Text HashableScriptData
parseScriptData what hexText = do
  bytes <- first (\e -> "invalid " <> what <> " hex: " <> T.pack e) (Base16.decode (TE.encodeUtf8 hexText))
  first (\e -> "invalid " <> what <> " CBOR: " <> T.pack (show e)) $
    deserialiseFromCBOR AsHashableScriptData bytes

-- | Parse a 28-byte payment key hash from hex.
parsePkh :: Text -> Either Text (Hash PaymentKey)
parsePkh hexText =
  first (\e -> "invalid required-signer pkh \"" <> hexText <> "\": " <> T.pack (show e)) $
    (deserialiseFromRawBytesHex (TE.encodeUtf8 hexText) :: Either RawBytesHexError (Hash PaymentKey))

-- | Parse the ledger protocol parameters JSON served by hydra-node's
-- @GET /protocol-parameters@.
parsePParams :: Text -> Either Text (PParams LedgerEra)
parsePParams t =
  first (\e -> "invalid protocol parameters JSON: " <> T.pack e) $
    Aeson.eitherDecodeStrict (TE.encodeUtf8 t)

-- | Deserialise a Plutus V3 text-envelope JSON value (@type@,
-- @description@, @cborHex@) into the validator script.
parsePlutusEnvelope :: Aeson.Value -> Either Text PlutusScript
parsePlutusEnvelope v = do
  te <- case Aeson.fromJSON v of
    Aeson.Error e -> Left ("invalid plutus envelope JSON: " <> T.pack e)
    Aeson.Success te -> Right te
  first (\e -> "invalid plutus envelope: " <> T.pack (show e)) $
    (deserialiseFromTextEnvelope te :: Either TextEnvelopeError PlutusScript)
