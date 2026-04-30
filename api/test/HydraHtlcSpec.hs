module HydraHtlcSpec (spec) where

import Codec.Binary.Bech32 qualified as Bech32
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hydra.Htlc
import Test.Hspec

spec :: Spec
spec = do
  describe "Plutus Data CBOR encoders" $ do
    it "encodes the HTLC datum as Constr 0 (tag 121)" $ do
      -- Constr 0 [bytes(32 0x00), int 100, bytes(28 0x00), bytes(28 0x00)]
      -- CBOR: D8 79 (tag 121)
      --       84       (array of 4)
      --       58 20 ...32 bytes...
      --       18 64    (uint 100)
      --       58 1C ...28 bytes...
      --       58 1C ...28 bytes...
      let hashB = BS.replicate 32 0
          pkhB = BS.replicate 28 0
          out = mkDatumCbor hashB 100 pkhB pkhB
          expected =
            "d87984"
              <> "5820"
              <> hex32Zeros
              <> "1864"
              <> "581c"
              <> hex28Zeros
              <> "581c"
              <> hex28Zeros
      hexEncode out `shouldBe` expected

    it "encodes a Claim redeemer as Constr 0 around the preimage bytes" $ do
      let preimage = decodeHex "deadbeef"
          out = mkClaimRedeemerCbor preimage
      -- D8 79 (tag 121) 81 (array 1) 44 (bytes 4) DEADBEEF
      hexEncode out `shouldBe` "d87981" <> "44" <> "deadbeef"

    it "encodes a Refund redeemer as Constr 1 with no fields" $ do
      -- D8 7A (tag 122) 80 (empty array)
      hexEncode refundRedeemerCbor `shouldBe` "d87a80"

  describe "addressOrPkhToBytes" $ do
    it "accepts a 56-char hex pkh and returns 28 bytes" $ do
      let pkhHex = "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c" :: Text
      addressOrPkhToBytes pkhHex
        `shouldBe` Right (decodeHex "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c")

    it "rejects a hex string that decodes to the wrong length" $ do
      addressOrPkhToBytes "0102030405" `shouldSatisfy` isLeft

    it "extracts the pkh from a testnet enterprise vkey address" $ do
      -- header 0x60 (testnet enterprise, payment vkey only) + 28-byte pkh.
      let pkhB = BS.pack [1 .. 28]
          addrBytes = BS.cons 0x60 pkhB
          addr = encodeBech32 "addr_test" addrBytes
      addressOrPkhToBytes addr `shouldBe` Right pkhB

    it "extracts the pkh from a mainnet base address (type 0)" $ do
      -- header 0x01 (mainnet base, payment vkey + stake vkey) + 28 + 28.
      let payment = BS.pack [11 .. 38]
          stake = BS.pack [50 .. 77]
          addrBytes = BS.cons 0x01 (payment <> stake)
          addr = encodeBech32 "addr" addrBytes
      addressOrPkhToBytes addr `shouldBe` Right payment

    it "rejects a script enterprise address (type 7)" $ do
      let scriptHash = BS.pack [1 .. 28]
          addrBytes = BS.cons 0x70 scriptHash
          addr = encodeBech32 "addr_test" addrBytes
      addressOrPkhToBytes addr `shouldSatisfy` isLeft

  describe "htlcScriptAddress" $ do
    it "produces a testnet bech32 addr_test address for Preview" $ do
      case htlcScriptAddress "Preview" of
        Right a -> "addr_test1" `T.isPrefixOf` a `shouldBe` True
        Left e -> expectationFailure ("expected Right, got Left " <> show e)

    it "produces a mainnet bech32 addr address for Mainnet" $ do
      case htlcScriptAddress "Mainnet" of
        Right a -> "addr1" `T.isPrefixOf` a `shouldBe` True
        Left e -> expectationFailure ("expected Right, got Left " <> show e)

    it "round-trips: decoding the produced address recovers header+scriptHash" $ do
      let Right addr = htlcScriptAddress "Preview"
      case Bech32.decodeLenient addr of
        Left e -> expectationFailure ("bech32 decode failed: " <> show e)
        Right (_, dp) -> case Bech32.dataPartToBytes dp of
          Nothing -> expectationFailure "bech32 data part not byte-aligned"
          Just bs -> do
            BS.length bs `shouldBe` 29
            BS.head bs `shouldBe` 0x70
            let scriptHash = BS.drop 1 bs
            scriptHash `shouldBe` decodeHex "81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df"

    it "rejects unknown networks" $ do
      htlcScriptAddress "BogusNet" `shouldSatisfy` isLeft

-- ─── helpers ───

hex32Zeros :: Text
hex32Zeros = "0000000000000000000000000000000000000000000000000000000000000000"

hex28Zeros :: Text
hex28Zeros = "00000000000000000000000000000000000000000000000000000000"

decodeHex :: Text -> ByteString
decodeHex t = case Base16.decode (TE.encodeUtf8 t) of
  Right b -> b
  Left e -> error $ "Test fixture hex decode failed: " <> e

encodeBech32 :: Text -> ByteString -> Text
encodeBech32 hrpText bs =
  let Right hrp = Bech32.humanReadablePartFromText hrpText
   in Bech32.encodeLenient hrp (Bech32.dataPartFromBytes bs)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
