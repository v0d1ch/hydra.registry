module TxBuilderSpec (spec) where

import Codec.Binary.Bech32 qualified as Bech32
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.List (isInfixOf)
import Data.Text.Encoding qualified as T
import Test.Hspec
import Tx.Builder
  ( BuildResult (..)
  , ClaimArgs (..)
  , LockArgs (..)
  , PublishRefArgs (..)
  , RefundArgs (..)
  , claimTxArgs
  , extractPkhFromAddress
  , htlcExecUnits
  , lockTxArgs
  , publishRefTxArgs
  , refundTxArgs
  )

-- | Convenient hand-rolled fixtures so each test is readable.
sampleLockArgs :: LockArgs
sampleLockArgs =
  LockArgs
    { inputUtxo = "aaaa#0"
    , inputLovelace = 22_000_000
    , walletAddress = "addr_test1walletXXX"
    , scriptAddress = "addr_test1scriptYYY"
    , datumCborHex = "ff"
    , sharedRefUtxo = Nothing
    , lockAmount = 10_000_000
    , validityUpperSlot = 12_345
    , requiredSignerPkhHex = "1052386136b347f3bb7c67fe3f2ee4ef120e1836e5d2707bb068afa6"
    , feeLovelace = 300_000
    , protocolParamsJson = "{}"
    , plutusEnvelope = Just (Aeson.Object mempty)
    }

sampleClaimArgs :: ClaimArgs
sampleClaimArgs =
  ClaimArgs
    { htlcInputTxHash = "bbbb"
    , htlcInputIndex = 0
    , refScriptUtxo = "cccc#0"
    , redeemerCborHex = "ff"
    , collateralUtxo = "dddd#1"
    , collateralLovelace = 13_400_000
    , totalCollateralLovelace = 2_250_000
    , walletAddress = "addr_test1ida"
    , htlcOutputLovelace = 10_000_000
    , validityUpperSlot = 12_345
    , requiredSignerPkhHex = "f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d"
    , feeLovelace = 1_500_000
    , protocolParamsJson = "{}"
    }

sampleRefundArgs :: RefundArgs
sampleRefundArgs =
  RefundArgs
    { htlcInputTxHash = "bbbb"
    , htlcInputIndex = 0
    , refScriptUtxo = "cccc#0"
    , redeemerCborHex = "ff"
    , collateralUtxo = "dddd#1"
    , collateralLovelace = 13_400_000
    , totalCollateralLovelace = 2_250_000
    , walletAddress = "addr_test1bob"
    , htlcOutputLovelace = 10_000_000
    , validityLowerSlot = 99_999
    , requiredSignerPkhHex = "1052386136b347f3bb7c67fe3f2ee4ef120e1836e5d2707bb068afa6"
    , feeLovelace = 1_500_000
    , protocolParamsJson = "{}"
    }

samplePublishRefArgs :: PublishRefArgs
samplePublishRefArgs =
  PublishRefArgs
    { inputUtxo = "eeee#1"
    , inputLovelace = 19_700_000
    , walletAddress = "addr_test1ida"
    , refOutputLovelace = 6_000_000
    , feeLovelace = 300_000
    , protocolParamsJson = "{}"
    , plutusEnvelope = Aeson.Object mempty
    }

contains :: String -> [String] -> Bool
contains needle = any (needle `isInfixOf`)

shouldContain' :: [String] -> String -> Expectation
shouldContain' xs needle = xs `shouldSatisfy` contains needle

spec :: Spec
spec = describe "Tx.Builder" $ do
  describe "lockTxArgs" $ do
    it "starts with `conway transaction build-raw`" $ do
      let args = lockTxArgs sampleLockArgs "/tmp/datum.cbor" "/tmp/htlc.json" "/tmp/pp.json" "/tmp/out.tx"
      take 3 args `shouldBe` ["conway", "transaction", "build-raw"]

    it "includes the input UTxO and the script-address output" $ do
      let args = lockTxArgs sampleLockArgs "/tmp/datum.cbor" "/tmp/htlc.json" "/tmp/pp.json" "/tmp/out.tx"
      args `shouldContain'` "aaaa#0"
      args `shouldContain'` "addr_test1scriptYYY+10000000"

    it "balances change to the wallet address (input − lock − fee)" $ do
      let args = lockTxArgs sampleLockArgs "/tmp/datum.cbor" "/tmp/htlc.json" "/tmp/pp.json" "/tmp/out.tx"
      -- 22_000_000 − 10_000_000 − 300_000 = 11_700_000
      args `shouldContain'` "addr_test1walletXXX+11700000"

    it "inlines the plutus envelope when no shared ref UTxO is published" $ do
      let args = lockTxArgs sampleLockArgs "/tmp/datum.cbor" "/tmp/htlc.json" "/tmp/pp.json" "/tmp/out.tx"
      args `shouldContain'` "--tx-out-reference-script-file"
      args `shouldContain'` "/tmp/htlc.json"

    it "omits the inline ref-script when a shared ref UTxO is set" $ do
      let args = lockTxArgs (sampleLockArgs{sharedRefUtxo = Just "aaaa#0"}) "/tmp/d.cbor" "/tmp/h.json" "/tmp/pp.json" "/tmp/o.tx"
      args `shouldNotSatisfy` contains "--tx-out-reference-script-file"
      args `shouldNotSatisfy` contains "/tmp/h.json"

    it "passes the validity-upper, required-signer, fee, and pp-file flags" $ do
      let args = lockTxArgs sampleLockArgs "/tmp/datum.cbor" "/tmp/htlc.json" "/tmp/pp.json" "/tmp/out.tx"
      args `shouldContain'` "--invalid-hereafter"
      args `shouldContain'` "12345"
      args `shouldContain'` "--required-signer-hash"
      args `shouldContain'` "1052386136b347f3bb7c67fe3f2ee4ef120e1836e5d2707bb068afa6"
      args `shouldContain'` "--fee"
      args `shouldContain'` "300000"
      args `shouldContain'` "--protocol-params-file"
      args `shouldContain'` "/tmp/pp.json"
      args `shouldContain'` "--out-file"
      args `shouldContain'` "/tmp/out.tx"

  describe "claimTxArgs" $ do
    it "uses `--spending-tx-in-reference` against the head's published ref UTxO" $ do
      let args = claimTxArgs sampleClaimArgs "/tmp/redeemer.cbor" "/tmp/pp.json" "/tmp/out.tx"
      args `shouldContain'` "--spending-tx-in-reference"
      args `shouldContain'` "cccc#0"

    it "passes the redeemer file and the inline-datum-present flag" $ do
      let args = claimTxArgs sampleClaimArgs "/tmp/redeemer.cbor" "/tmp/pp.json" "/tmp/out.tx"
      args `shouldContain'` "--spending-reference-tx-in-redeemer-cbor-file"
      args `shouldContain'` "/tmp/redeemer.cbor"
      args `shouldContain'` "--spending-reference-tx-in-inline-datum-present"

    it "encodes execution units as `(steps,mem)` per cardano-cli's quirk" $ do
      let args = claimTxArgs sampleClaimArgs "/tmp/r.cbor" "/tmp/pp.json" "/tmp/o.tx"
          (steps, mem) = htlcExecUnits
      args `shouldContain'` ("(" <> show steps <> "," <> show mem <> ")")

    it "includes collateral input, return-collateral output, and total-collateral" $ do
      let args = claimTxArgs sampleClaimArgs "/tmp/r.cbor" "/tmp/pp.json" "/tmp/o.tx"
      args `shouldContain'` "--tx-in-collateral"
      args `shouldContain'` "dddd#1"
      args `shouldContain'` "--tx-out-return-collateral"
      -- 13_400_000 − 2_250_000 = 11_150_000
      args `shouldContain'` "addr_test1ida+11150000"
      args `shouldContain'` "--tx-total-collateral"
      args `shouldContain'` "2250000"

    it "puts the claim output value at `htlcOutputLovelace − fee`" $ do
      let args = claimTxArgs sampleClaimArgs "/tmp/r.cbor" "/tmp/pp.json" "/tmp/o.tx"
      -- 10_000_000 − 1_500_000 = 8_500_000
      args `shouldContain'` "addr_test1ida+8500000"

    it "uses --invalid-hereafter (claim must happen *before* timeout)" $ do
      let args = claimTxArgs sampleClaimArgs "/tmp/r.cbor" "/tmp/pp.json" "/tmp/o.tx"
      args `shouldContain'` "--invalid-hereafter"
      args `shouldNotSatisfy` contains "--invalid-before"

  describe "refundTxArgs" $ do
    it "uses --invalid-before (refund must happen *after* timeout)" $ do
      let args = refundTxArgs sampleRefundArgs "/tmp/r.cbor" "/tmp/pp.json" "/tmp/o.tx"
      args `shouldContain'` "--invalid-before"
      args `shouldNotSatisfy` contains "--invalid-hereafter"

    it "still uses the same Plutus + collateral structure as claim" $ do
      let args = refundTxArgs sampleRefundArgs "/tmp/r.cbor" "/tmp/pp.json" "/tmp/o.tx"
      args `shouldContain'` "--spending-plutus-script-v3"
      args `shouldContain'` "--spending-tx-in-reference"
      args `shouldContain'` "cccc#0"
      args `shouldContain'` "--tx-in-collateral"

  describe "publishRefTxArgs" $ do
    it "produces a normal tx with two outputs (ref + change), no Plutus flags" $ do
      let args = publishRefTxArgs samplePublishRefArgs "/tmp/htlc.json" "/tmp/pp.json" "/tmp/out.tx"
      args `shouldNotSatisfy` contains "--spending-plutus-script-v3"
      args `shouldNotSatisfy` contains "--tx-in-collateral"
      args `shouldContain'` "addr_test1ida+6000000"
      -- 19_700_000 − 6_000_000 − 300_000 = 13_400_000
      args `shouldContain'` "addr_test1ida+13400000"
      args `shouldContain'` "--tx-out-reference-script-file"
      args `shouldContain'` "/tmp/htlc.json"

  describe "BuildResult JSON serialisation" $ do
    let sampleEnvelope =
          Aeson.object
            [ "type"        Aeson..= ("Tx ConwayEra" :: String)
            , "description" Aeson..= ("Ledger Cddl Format" :: String)
            , "cborHex"     Aeson..= ("deadbeef" :: String)
            ]
        result = BuildResult
          { cborHex  = "deadbeef"
          , txId     = "abc123"
          , envelope = sampleEnvelope
          }

    it "serialises as a flat object (cardano-cli text-envelope shape)" $ do
      case Aeson.toJSON result of
        Aeson.Object km -> do
          KM.lookup "type"    km `shouldBe` Just (Aeson.String "Tx ConwayEra")
          KM.lookup "cborHex" km `shouldBe` Just (Aeson.String "deadbeef")
          KM.lookup "txId"    km `shouldBe` Just (Aeson.String "abc123")
        other -> expectationFailure $ "Expected Object, got: " <> show other

    it "does not nest an 'envelope' key" $ do
      case Aeson.toJSON result of
        Aeson.Object km -> KM.lookup "envelope" km `shouldBe` Nothing
        other -> expectationFailure $ "Expected Object, got: " <> show other

  describe "extractPkhFromAddress" $ do
    it "extracts payment key hash from bech32 enterprise testnet address" $ do
      let knownPkh = BS.replicate 28 0xab
          addrBytes = BS.cons 0x60 knownPkh
          Right hrp = Bech32.humanReadablePartFromText "addr_test"
          dp = Bech32.dataPartFromBytes addrBytes
          addr = Bech32.encodeLenient hrp dp
      extractPkhFromAddress addr `shouldBe` Right (T.decodeUtf8 $ Base16.encode knownPkh)

    it "rejects an address whose decoded payload is too short" $ do
      let shortBytes = BS.replicate 10 0x00
          Right hrp = Bech32.humanReadablePartFromText "addr_test"
          dp = Bech32.dataPartFromBytes shortBytes
          addr = Bech32.encodeLenient hrp dp
      extractPkhFromAddress addr `shouldBe` Left "address too short"

    it "rejects invalid bech32 input" $ do
      extractPkhFromAddress "not-bech32-at-all!!!" `shouldBe` Left "invalid bech32 address"
