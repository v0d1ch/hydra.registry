-- | Tests for the native (cardano-api based) HTLC transaction builder.
--
-- Every test decodes the produced CBOR back into a 'Tx' and asserts on
-- the actual transaction body — inputs, outputs, datums, fees, validity
-- bounds, collateral and redeemer budgets — rather than on any
-- intermediate representation.
module TxBuilderSpec (spec) where

import Cardano.Ledger.Alonzo.Scripts qualified as Ledger (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger (unRedeemers)
import Codec.Binary.Bech32 qualified as Bech32
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Word (Word8)
import Hydra.Cardano.Api
import Hydra.Htlc (hexEncode, mkClaimRedeemerCbor, mkDatumCbor, refundRedeemerCbor)
import Test.Hspec
import Tx.Builder
  ( BuildResult (..)
  , ClaimArgs (..)
  , LockArgs (..)
  , PublishRefArgs (..)
  , RefundArgs (..)
  , buildClaimTx
  , buildLockTx
  , buildPublishRefTx
  , buildRefundTx
  , extractPkhFromAddress
  , htlcExecUnits
  )
import Prelude

-- ─── address / hash fixtures ──────────────────────────────────────────

-- | A syntactically valid bech32 testnet address with the given header
-- byte and 28-byte payload.
mkBech32Addr :: Word8 -> ByteString -> Text
mkBech32Addr header payload =
  let Right hrp = Bech32.humanReadablePartFromText "addr_test"
   in Bech32.encodeLenient hrp (Bech32.dataPartFromBytes (BS.cons header payload))

-- 0x60 = testnet enterprise address, payment credential is a key hash.
walletAddrText :: Text
walletAddrText = mkBech32Addr 0x60 (BS.replicate 28 0xab)

claimerAddrText :: Text
claimerAddrText = mkBech32Addr 0x60 (BS.replicate 28 0xcd)

-- 0x70 = testnet enterprise address, payment credential is a script hash.
scriptAddrText :: Text
scriptAddrText = mkBech32Addr 0x70 (BS.replicate 28 0x81)

senderPkhHex :: Text
senderPkhHex = "1052386136b347f3bb7c67fe3f2ee4ef120e1836e5d2707bb068afa6"

claimerPkhHex :: Text
claimerPkhHex = "f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d"

lockInputHash, htlcTxHash, refTxHash, collTxHash, pubInputHash :: Text
lockInputHash = T.replicate 64 "a"
htlcTxHash = T.replicate 64 "b"
refTxHash = T.replicate 64 "c"
collTxHash = T.replicate 64 "d"
pubInputHash = T.replicate 64 "e"

datumHex :: Text
datumHex =
  hexEncode $
    mkDatumCbor (BS.replicate 32 0x01) 42_000 (BS.replicate 28 0x02) (BS.replicate 28 0x03)

claimRedeemerHex :: Text
claimRedeemerHex = hexEncode (mkClaimRedeemerCbor (BS.replicate 32 0x04))

refundRedeemerHex :: Text
refundRedeemerHex = hexEncode refundRedeemerCbor

-- ─── decoding helpers ─────────────────────────────────────────────────

unsafeTxIn :: Text -> Word -> TxIn
unsafeTxIn h ix =
  case deserialiseFromRawBytesHex (TE.encodeUtf8 h) :: Either RawBytesHexError TxId of
    Left e -> error ("unsafeTxIn: " <> show e)
    Right tid -> TxIn tid (TxIx ix)

unsafePkh :: Text -> Hash PaymentKey
unsafePkh h =
  case deserialiseFromRawBytesHex (TE.encodeUtf8 h) :: Either RawBytesHexError (Hash PaymentKey) of
    Left e -> error ("unsafePkh: " <> show e)
    Right pkh -> pkh

unsafeAddr :: Text -> AddressInEra
unsafeAddr t =
  maybe (error ("unsafeAddr: " <> T.unpack t)) id $
    deserialiseAddress (AsAddressInEra AsConwayEra) t

-- | Decode the built CBOR back into a 'Tx' and check that the reported
-- txId matches the body hash.
decodeBuilt :: Either Text BuildResult -> IO (BuildResult, Tx)
decodeBuilt = \case
  Left e -> fail ("expected Right BuildResult, got Left: " <> T.unpack e)
  Right br -> do
    bytes <- either fail pure (Base16.decode (TE.encodeUtf8 br.cborHex))
    tx <-
      either (fail . show) pure $
        deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes
    br.txId `shouldBe` serialiseToRawBytesHexText (getTxId (getTxBody tx))
    pure (br, tx)

contentOf :: Tx -> TxBodyContent ViewTx
contentOf = getTxBodyContent . getTxBody

-- | All @(datum, exUnits)@ pairs of the tx's redeemers.
redeemerUnits :: Tx -> [Ledger.ExUnits]
redeemerUnits tx =
  case getTxBody tx of
    ShelleyTxBody{txBodyScriptData} ->
      case txBodyScriptData of
        TxBodyNoScriptData -> []
        TxBodyScriptData _ redeemers -> map snd (Map.elems (Ledger.unRedeemers redeemers))

-- ─── sample args ──────────────────────────────────────────────────────

sampleLockArgs :: Aeson.Value -> Text -> LockArgs
sampleLockArgs plutusEnv ppJson =
  LockArgs
    { inputUtxo = lockInputHash <> "#0"
    , inputLovelace = 22_000_000
    , walletAddress = walletAddrText
    , scriptAddress = scriptAddrText
    , datumCborHex = datumHex
    , sharedRefUtxo = Nothing
    , lockAmount = 10_000_000
    , validityUpperSlot = 12_345
    , requiredSignerPkhHex = senderPkhHex
    , feeLovelace = 300_000
    , protocolParamsJson = ppJson
    , plutusEnvelope = Just plutusEnv
    }

sampleClaimArgs :: Text -> ClaimArgs
sampleClaimArgs ppJson =
  ClaimArgs
    { htlcInputTxHash = htlcTxHash
    , htlcInputIndex = 0
    , refScriptUtxo = refTxHash <> "#0"
    , redeemerCborHex = claimRedeemerHex
    , collateralUtxo = collTxHash <> "#1"
    , collateralLovelace = 13_400_000
    , totalCollateralLovelace = 2_250_000
    , walletAddress = claimerAddrText
    , htlcOutputLovelace = 10_000_000
    , validityUpperSlot = 12_345
    , requiredSignerPkhHex = claimerPkhHex
    , feeLovelace = 1_500_000
    , protocolParamsJson = ppJson
    }

sampleRefundArgs :: Text -> RefundArgs
sampleRefundArgs ppJson =
  RefundArgs
    { htlcInputTxHash = htlcTxHash
    , htlcInputIndex = 0
    , refScriptUtxo = refTxHash <> "#0"
    , redeemerCborHex = refundRedeemerHex
    , collateralUtxo = collTxHash <> "#1"
    , collateralLovelace = 13_400_000
    , totalCollateralLovelace = 2_250_000
    , walletAddress = walletAddrText
    , htlcOutputLovelace = 10_000_000
    , validityLowerSlot = 99_999
    , requiredSignerPkhHex = senderPkhHex
    , feeLovelace = 1_500_000
    , protocolParamsJson = ppJson
    }

samplePublishRefArgs :: Aeson.Value -> Text -> PublishRefArgs
samplePublishRefArgs plutusEnv ppJson =
  PublishRefArgs
    { inputUtxo = pubInputHash <> "#1"
    , inputLovelace = 19_700_000
    , walletAddress = walletAddrText
    , refOutputLovelace = 6_000_000
    , feeLovelace = 300_000
    , protocolParamsJson = ppJson
    , plutusEnvelope = plutusEnv
    }

-- ─── spec ─────────────────────────────────────────────────────────────

spec :: Spec
spec = do
  ppJson <- runIO $ TIO.readFile "test/fixtures/protocol-parameters.json"
  plutusEnv <-
    runIO $
      Aeson.eitherDecodeFileStrict @Aeson.Value "test/fixtures/always-fails.plutus"
        >>= either fail pure

  describe "buildLockTx" $ do
    it "spends the wallet input into a script output with the inline HTLC datum" $ do
      (_, tx) <- decodeBuilt (buildLockTx (sampleLockArgs plutusEnv ppJson))
      let content = contentOf tx
      map fst (txIns content) `shouldBe` [unsafeTxIn lockInputHash 0]
      case txOuts content of
        (TxOut addr value datum _refScript : _) -> do
          addr `shouldBe` unsafeAddr scriptAddrText
          value `shouldBe` lovelaceToValue (Coin 10_000_000)
          expected <-
            either (fail . show) pure . deserialiseFromCBOR AsHashableScriptData
              =<< either fail pure (Base16.decode (TE.encodeUtf8 datumHex))
          datum `shouldBe` TxOutDatumInline expected
        outs -> fail ("expected at least one output, got " <> show (length outs))

    it "inlines the plutus reference script when no shared ref UTxO is published" $ do
      (_, tx) <- decodeBuilt (buildLockTx (sampleLockArgs plutusEnv ppJson))
      case txOuts (contentOf tx) of
        (TxOut _ _ _ refScript : _) -> refScript `shouldNotBe` ReferenceScriptNone
        outs -> fail ("expected at least one output, got " <> show (length outs))

    it "omits the reference script when a shared ref UTxO is set" $ do
      let args = (sampleLockArgs plutusEnv ppJson){sharedRefUtxo = Just (refTxHash <> "#0")}
      (_, tx) <- decodeBuilt (buildLockTx args)
      case txOuts (contentOf tx) of
        (TxOut _ _ _ refScript : _) -> refScript `shouldBe` ReferenceScriptNone
        outs -> fail ("expected at least one output, got " <> show (length outs))

    it "balances change to the wallet address (input − lock − fee)" $ do
      (_, tx) <- decodeBuilt (buildLockTx (sampleLockArgs plutusEnv ppJson))
      case txOuts (contentOf tx) of
        [_, TxOut addr value _ _] -> do
          addr `shouldBe` unsafeAddr walletAddrText
          -- 22_000_000 − 10_000_000 − 300_000 = 11_700_000
          value `shouldBe` lovelaceToValue (Coin 11_700_000)
        outs -> fail ("expected exactly two outputs, got " <> show (length outs))

    it "sets explicit fee, upper validity bound, and the required signer" $ do
      (_, tx) <- decodeBuilt (buildLockTx (sampleLockArgs plutusEnv ppJson))
      let content = contentOf tx
      txFee content `shouldBe` TxFeeExplicit (Coin 300_000)
      txValidityUpperBound content `shouldBe` TxValidityUpperBound (SlotNo 12_345)
      txValidityLowerBound content `shouldBe` TxValidityNoLowerBound
      txExtraKeyWits content `shouldBe` TxExtraKeyWitnesses [unsafePkh senderPkhHex]

    it "produces a lock tx with no script data (nothing is executed)" $ do
      (_, tx) <- decodeBuilt (buildLockTx (sampleLockArgs plutusEnv ppJson))
      redeemerUnits tx `shouldBe` []

    it "wraps the tx in a Conway text envelope carrying the same cborHex" $ do
      (br, _) <- decodeBuilt (buildLockTx (sampleLockArgs plutusEnv ppJson))
      case br.envelope of
        Aeson.Object km -> do
          KM.lookup (Key.fromString "type") km `shouldBe` Just (Aeson.String "Tx ConwayEra")
          KM.lookup (Key.fromString "cborHex") km `shouldBe` Just (Aeson.String br.cborHex)
        other -> fail ("expected envelope object, got: " <> show other)

    it "fails when neither a shared ref UTxO nor a plutus envelope is supplied" $ do
      let args = (sampleLockArgs plutusEnv ppJson){sharedRefUtxo = Nothing, plutusEnvelope = Nothing}
      case buildLockTx args of
        Left e -> e `shouldSatisfy` T.isInfixOf "no shared ref-script"
        Right _ -> expectationFailure "expected Left, got Right"

    it "fails on an invalid wallet address" $ do
      let args = (sampleLockArgs plutusEnv ppJson :: LockArgs){walletAddress = "not-an-address"}
      buildLockTx args `shouldSatisfy` isLeft

    it "fails on invalid datum CBOR hex" $ do
      let args = (sampleLockArgs plutusEnv ppJson){datumCborHex = "zz"}
      buildLockTx args `shouldSatisfy` isLeft

    it "fails on a malformed input UTxO reference" $ do
      let args = (sampleLockArgs plutusEnv ppJson :: LockArgs){inputUtxo = "nope"}
      buildLockTx args `shouldSatisfy` isLeft

  describe "buildClaimTx" $ do
    it "spends the HTLC input against the published reference script UTxO" $ do
      (_, tx) <- decodeBuilt (buildClaimTx (sampleClaimArgs ppJson))
      let content = contentOf tx
      map fst (txIns content) `shouldBe` [unsafeTxIn htlcTxHash 0]
      case txInsReference content of
        TxInsReference refIns _ -> refIns `shouldBe` [unsafeTxIn refTxHash 0]
        TxInsReferenceNone -> expectationFailure "expected reference inputs, got none"

    it "executes exactly one redeemer with the fixed HTLC budget" $ do
      (_, tx) <- decodeBuilt (buildClaimTx (sampleClaimArgs ppJson))
      let (steps, mem) = htlcExecUnits
      redeemerUnits tx
        `shouldBe` [Ledger.ExUnits{Ledger.exUnitsMem = fromInteger mem, Ledger.exUnitsSteps = fromInteger steps}]

    it "sets collateral input, total collateral, and return collateral" $ do
      (_, tx) <- decodeBuilt (buildClaimTx (sampleClaimArgs ppJson))
      let content = contentOf tx
      txInsCollateral content `shouldBe` TxInsCollateral [unsafeTxIn collTxHash 1]
      txTotalCollateral content `shouldBe` TxTotalCollateral babbageBasedEra (Coin 2_250_000)
      case txReturnCollateral content of
        TxReturnCollateral _ (TxOut addr value _ _) -> do
          addr `shouldBe` unsafeAddr claimerAddrText
          -- 13_400_000 − 2_250_000 = 11_150_000
          value `shouldBe` lovelaceToValue (Coin 11_150_000)
        TxReturnCollateralNone -> expectationFailure "expected return collateral, got none"

    it "pays htlcOutputLovelace − fee to the claimer wallet" $ do
      (_, tx) <- decodeBuilt (buildClaimTx (sampleClaimArgs ppJson))
      case txOuts (contentOf tx) of
        [TxOut addr value _ _] -> do
          addr `shouldBe` unsafeAddr claimerAddrText
          -- 10_000_000 − 1_500_000 = 8_500_000
          value `shouldBe` lovelaceToValue (Coin 8_500_000)
        outs -> fail ("expected exactly one output, got " <> show (length outs))

    it "uses an upper validity bound only (claim happens before the timeout)" $ do
      (_, tx) <- decodeBuilt (buildClaimTx (sampleClaimArgs ppJson))
      let content = contentOf tx
      txValidityUpperBound content `shouldBe` TxValidityUpperBound (SlotNo 12_345)
      txValidityLowerBound content `shouldBe` TxValidityNoLowerBound
      txExtraKeyWits content `shouldBe` TxExtraKeyWitnesses [unsafePkh claimerPkhHex]

    it "fails when the protocol parameters JSON does not parse" $ do
      let args = (sampleClaimArgs ppJson :: ClaimArgs){protocolParamsJson = "{"}
      buildClaimTx args `shouldSatisfy` isLeft

  describe "buildRefundTx" $ do
    it "uses a lower validity bound only (refund happens after the timeout)" $ do
      (_, tx) <- decodeBuilt (buildRefundTx (sampleRefundArgs ppJson))
      let content = contentOf tx
      txValidityLowerBound content `shouldBe` TxValidityLowerBound (SlotNo 99_999)
      txValidityUpperBound content `shouldBe` TxValidityNoUpperBound

    it "keeps the same reference-script and collateral structure as claim" $ do
      (_, tx) <- decodeBuilt (buildRefundTx (sampleRefundArgs ppJson))
      let content = contentOf tx
          (steps, mem) = htlcExecUnits
      case txInsReference content of
        TxInsReference refIns _ -> refIns `shouldBe` [unsafeTxIn refTxHash 0]
        TxInsReferenceNone -> expectationFailure "expected reference inputs, got none"
      txInsCollateral content `shouldBe` TxInsCollateral [unsafeTxIn collTxHash 1]
      redeemerUnits tx
        `shouldBe` [Ledger.ExUnits{Ledger.exUnitsMem = fromInteger mem, Ledger.exUnitsSteps = fromInteger steps}]

  describe "buildPublishRefTx" $ do
    it "publishes the script in the first output and returns change in the second" $ do
      (_, tx) <- decodeBuilt (buildPublishRefTx (samplePublishRefArgs plutusEnv ppJson))
      case txOuts (contentOf tx) of
        [TxOut refAddr refValue _ refScript, TxOut changeAddr changeValue _ changeRef] -> do
          refAddr `shouldBe` unsafeAddr walletAddrText
          refValue `shouldBe` lovelaceToValue (Coin 6_000_000)
          refScript `shouldNotBe` ReferenceScriptNone
          changeAddr `shouldBe` unsafeAddr walletAddrText
          -- 19_700_000 − 6_000_000 − 300_000 = 13_400_000
          changeValue `shouldBe` lovelaceToValue (Coin 13_400_000)
          changeRef `shouldBe` ReferenceScriptNone
        outs -> fail ("expected exactly two outputs, got " <> show (length outs))

    it "is a plain transaction: no collateral, no script data, no bounds, no signers" $ do
      (_, tx) <- decodeBuilt (buildPublishRefTx (samplePublishRefArgs plutusEnv ppJson))
      let content = contentOf tx
      -- the CBOR round-trip decodes an absent collateral set as an
      -- empty list, so accept both empty representations
      case txInsCollateral content of
        TxInsCollateralNone -> pure ()
        TxInsCollateral ins -> ins `shouldBe` []
      redeemerUnits tx `shouldBe` []
      txValidityLowerBound content `shouldBe` TxValidityNoLowerBound
      txValidityUpperBound content `shouldBe` TxValidityNoUpperBound
      txExtraKeyWits content `shouldBe` TxExtraKeyWitnessesNone

  describe "BuildResult JSON serialisation" $ do
    let sampleEnvelope =
          Aeson.object
            [ "type" Aeson..= ("Tx ConwayEra" :: String)
            , "description" Aeson..= ("Ledger Cddl Format" :: String)
            , "cborHex" Aeson..= ("deadbeef" :: String)
            ]
        result =
          BuildResult
            { cborHex = "deadbeef"
            , txId = "abc123"
            , envelope = sampleEnvelope
            }

    it "serialises as a flat object (cardano-cli text-envelope shape)" $ do
      case Aeson.toJSON result of
        Aeson.Object km -> do
          KM.lookup "type" km `shouldBe` Just (Aeson.String "Tx ConwayEra")
          KM.lookup "cborHex" km `shouldBe` Just (Aeson.String "deadbeef")
          KM.lookup "txId" km `shouldBe` Just (Aeson.String "abc123")
        other -> expectationFailure $ "Expected Object, got: " <> show other

    it "does not nest an 'envelope' key" $ do
      case Aeson.toJSON result of
        Aeson.Object km -> KM.lookup "envelope" km `shouldBe` Nothing
        other -> expectationFailure $ "Expected Object, got: " <> show other

  describe "extractPkhFromAddress" $ do
    it "extracts payment key hash from bech32 enterprise testnet address" $ do
      let knownPkh = BS.replicate 28 0xab
      extractPkhFromAddress walletAddrText
        `shouldBe` Right (TE.decodeUtf8 (Base16.encode knownPkh))

    it "rejects an address whose decoded payload is too short" $ do
      let shortBytes = BS.replicate 10 0x00
          Right hrp = Bech32.humanReadablePartFromText "addr_test"
          dp = Bech32.dataPartFromBytes shortBytes
          addr = Bech32.encodeLenient hrp dp
      extractPkhFromAddress addr `shouldBe` Left "address too short"

    it "rejects invalid bech32 input" $ do
      extractPkhFromAddress "not-bech32-at-all!!!" `shouldBe` Left "invalid bech32 address"

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
