module ValidationSpec (spec) where

import Api.Validation
import Data.Text qualified as T
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Api.Validation" $ do
  describe "validateAddress" $ do
    it "accepts valid bech32 mainnet address (addr1...)" $ do
      let addr = "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp"
      validateAddress addr `shouldBe` Right Bech32Address

    it "accepts valid bech32 testnet address (addr_test1...)" $ do
      let addr = "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqfjkjv7"
      validateAddress addr `shouldBe` Right Bech32Address

    it "accepts valid bech32 stake address (stake1...)" $ do
      let addr = "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
      validateAddress addr `shouldBe` Right Bech32Address

    it "accepts valid hex address" $ do
      -- 56 hex chars (minimum for a payment key hash)
      let addr = T.replicate 56 "a"
      validateAddress addr `shouldBe` Right HexAddress

    it "accepts valid base58 address (Byron)" $ do
      let addr = "DdzFFzCqrhsrcTVhLEz2bKqR2SFCGH5tHMVMc1kLyaBaL4rizGrX1msbfHmo"
      validateAddress addr `shouldBe` Right Base58Address

    it "rejects empty address" $ do
      validateAddress "" `shouldBe` Left "Empty address"

    it "rejects garbage input" $ do
      case validateAddress "not-a-valid-address!!!" of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected rejection of garbage input"

    it "rejects very short input" $ do
      case validateAddress "abc" of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected rejection of short input"

  describe "isBech32" $ do
    it "returns True for addr1 prefix" $ do
      isBech32 "addr1qxyz0test0value" `shouldBe` True

    it "returns True for addr_test1 prefix" $ do
      isBech32 "addr_test1qxyz0test0value" `shouldBe` True

    it "returns False for invalid characters after prefix" $ do
      isBech32 "addr1INVALID_CHARS" `shouldBe` False

    it "returns False for addr1 alone (too short)" $ do
      isBech32 "addr1" `shouldBe` False

  describe "isHexAddress" $ do
    it "accepts 56-char hex string" $ do
      isHexAddress (T.replicate 56 "f") `shouldBe` True

    it "accepts 130-char hex string" $ do
      isHexAddress (T.replicate 130 "0") `shouldBe` True

    it "rejects 55-char hex string (too short)" $ do
      isHexAddress (T.replicate 55 "a") `shouldBe` False

    it "rejects non-hex characters" $ do
      isHexAddress (T.replicate 56 "g") `shouldBe` False

  describe "isBase58" $ do
    it "accepts valid base58 characters" $ do
      isBase58 "DdzFFzCqrhsrcTVhLEz2bKqR2SFCGH5" `shouldBe` True

    it "rejects strings with 0 (not in base58)" $ do
      isBase58 (T.replicate 30 "0") `shouldBe` False

  describe "property tests" $ do
    it "valid bech32 addresses are non-empty" $
      property $ \(NonEmpty s) ->
        let addr = T.pack $ "addr1" <> filter isBech32Char s
         in T.length addr > 5 ==> validateAddress addr == Right Bech32Address

    it "hex addresses contain only hex digits" $
      property $ \(Positive n) ->
        let len = 56 + (n `mod` 75)
            addr = T.replicate len "a"
         in validateAddress addr == Right HexAddress
