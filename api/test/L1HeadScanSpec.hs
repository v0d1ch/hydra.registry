module L1HeadScanSpec (spec) where

import Data.ByteString.Base16 qualified as Base16
import Data.Maybe (fromJust)
import Data.Text.Encoding qualified as TE
import Db qualified
import Db.Schema (HeadParticipant (..))
import GHC.IsList (fromList)
import Hydra.Cardano.Api
import Hydra.Htlc (scriptAddressFromHash)
import L1.HeadScan
import Logging (newLogger)
import Logging qualified
import Test.Hspec
import TestUtils
import Prelude

-- | The 2.2.0 head validator address on Preprod, verified against the live
-- chain (68 open-head UTxOs at the time of writing).
headAddr22Preprod :: Text
headAddr22Preprod = "addr_test1wr7htcjvn653tn5wfrfl78gv2jksnnqprywzgst26ld6fgcn3usg2"

-- Test fixtures ---------------------------------------------------------

headPolicyHex :: Text
headPolicyHex = "f0dc8b9cf371642401b74375767631e83504bce2e26d5bc47c728d88"

foreignPolicyHex :: Text
foreignPolicyHex = "a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0"

pt1Hex, pt2Hex :: Text
pt1Hex = "80d997ce31b99dab96ce34983a1fdda7c9285fc3afc129c9032cdc20"
pt2Hex = "11d997ce31b99dab96ce34983a1fdda7c9285fc3afc129c9032cdc11"

unsafePolicyId :: Text -> PolicyId
unsafePolicyId t = either (error . show) id $ deserialiseFromRawBytesHex (TE.encodeUtf8 t)

hexAssetName :: Text -> AssetName
hexAssetName t = UnsafeAssetName (either error id $ Base16.decode (TE.encodeUtf8 t))

someAddr :: AddressInEra
someAddr = fromJust $ deserialiseAddress (AsAddressInEra AsConwayEra) headAddr22Preprod

mkOut :: Value -> TxOut CtxUTxO
mkOut v = TxOut someAddr v TxOutDatumNone ReferenceScriptNone

headOutput :: TxOut CtxUTxO
headOutput =
  mkOut $
    fromList
      [ (AdaAssetId, 2_103_280)
      , (AssetId (unsafePolicyId headPolicyHex) stAssetNameV2, 1)
      , (AssetId (unsafePolicyId headPolicyHex) (hexAssetName pt1Hex), 1)
      , (AssetId (unsafePolicyId headPolicyHex) (hexAssetName pt2Hex), 1)
      , (AssetId (unsafePolicyId foreignPolicyHex) (UnsafeAssetName "committedNFT"), 1)
      ]

spec :: Spec
spec = describe "L1.HeadScan" $ do
  describe "scriptAddressFromHash (head validator golden)" $
    it "derives the live-verified 2.2.0 Preprod address" $
      scriptAddressFromHash "fd75e24c9ea915ce8e48d3ff1d0c54ad09cc01191c24416ad7dba4a3" "Preprod"
        `shouldBe` Right headAddr22Preprod

  describe "extractHeads" $ do
    it "extracts headId, participants, and lovelace from a head UTxO" $ do
      case extractHeads [headOutput] of
        [r] -> do
          r.scanHeadId `shouldBe` headPolicyHex
          r.scanParticipants `shouldMatchList` [pt1Hex, pt2Hex]
          r.scanLovelace `shouldBe` 2_103_280
        other -> expectationFailure $ "expected one head, got: " <> show other

    it "ignores outputs without a Hydra state token" $ do
      let plain = mkOut $ fromList [(AdaAssetId, 5_000_000), (AssetId (unsafePolicyId foreignPolicyHex) (UnsafeAssetName "x"), 1)]
      extractHeads [plain] `shouldBe` []

    it "recognizes the legacy HydraHeadV1 state token" $ do
      let v1 =
            mkOut $
              fromList
                [ (AdaAssetId, 1_000_000)
                , (AssetId (unsafePolicyId headPolicyHex) stAssetNameV1, 1)
                , (AssetId (unsafePolicyId headPolicyHex) (hexAssetName pt1Hex), 1)
                ]
      case extractHeads [v1] of
        [r] -> r.scanParticipants `shouldBe` [pt1Hex]
        other -> expectationFailure $ "expected one head, got: " <> show other

    it "excludes non-28-byte token names from participants" $ do
      let weird =
            mkOut $
              fromList
                [ (AdaAssetId, 1_000_000)
                , (AssetId (unsafePolicyId headPolicyHex) stAssetNameV2, 1)
                , (AssetId (unsafePolicyId headPolicyHex) (UnsafeAssetName "short"), 1)
                , (AssetId (unsafePolicyId headPolicyHex) (hexAssetName pt1Hex), 1)
                ]
      case extractHeads [weird] of
        [r] -> r.scanParticipants `shouldBe` [pt1Hex]
        other -> expectationFailure $ "expected one head, got: " <> show other

  describe "headValidatorHashes" $
    it "covers all published versions from 0.13.0 through 2.3.0" $ do
      length headValidatorHashes `shouldSatisfy` (>= 14)
      -- spot-check the versions our fleet runs
      lookup "2.2.0" headValidatorHashes `shouldBe` Just "fd75e24c9ea915ce8e48d3ff1d0c54ad09cc01191c24416ad7dba4a3"
      lookup "2.3.0" headValidatorHashes `shouldBe` Just "2b91a7e666575a2465b8c7f6a7f960d5870cf13694a67f3215e014c5"

  describe "applyScanResults (integration)" $ around withTestPool $ do
    it "stores participants as onChainIds and TVL on the explorer head" $ \pool -> do
      Db.upsertExplorerHead pool headPolicyHex "Preprod" 1 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      applyScanResults (newLogger Logging.Error) pool [HeadScanResult headPolicyHex [pt1Hex, pt2Hex] 7_000_000]
      ps <- Db.getParticipantsForHead pool headPolicyHex
      map (\HeadParticipant{participantAddress = a} -> a) ps `shouldMatchList` [pt1Hex, pt2Hex]
      map (\HeadParticipant{participantOnChainId = o} -> o) ps `shouldMatchList` [Just pt1Hex, Just pt2Hex]
      (uniqueCount, _nets, tvl) <- Db.getExplorerStats pool
      uniqueCount `shouldBe` 2
      tvl `shouldBe` 7_000_000

    it "updates TVL on re-apply without duplicating participants" $ \pool -> do
      Db.upsertExplorerHead pool headPolicyHex "Preprod" 1 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      applyScanResults (newLogger Logging.Error) pool [HeadScanResult headPolicyHex [pt1Hex] 5_000_000]
      applyScanResults (newLogger Logging.Error) pool [HeadScanResult headPolicyHex [pt1Hex] 6_000_000]
      ps <- Db.getParticipantsForHead pool headPolicyHex
      length ps `shouldBe` 1
      (_, _, tvl) <- Db.getExplorerStats pool
      tvl `shouldBe` 6_000_000

    it "explorer upsert does not clobber scanned TVL" $ \pool -> do
      Db.upsertExplorerHead pool headPolicyHex "Preprod" 1 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      applyScanResults (newLogger Logging.Error) pool [HeadScanResult headPolicyHex [pt1Hex] 5_000_000]
      -- sidecar polls the explorer again and re-upserts the head row
      Db.upsertExplorerHead pool headPolicyHex "Preprod" 1 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      (_, _, tvl) <- Db.getExplorerStats pool
      tvl `shouldBe` 5_000_000
