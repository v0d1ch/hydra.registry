module Hydra.Htlc where

import Codec.Binary.Bech32 qualified as Bech32
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | The HTLC validator script hash as 28-byte hex.
htlcScriptHashHex :: Text
htlcScriptHashHex = "81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df"

-- | Compute the HTLC validator's bech32 script address for a network.
-- "Mainnet" → @addr1...@, "Preview"/"Preprod" → @addr_test1...@.
-- The address is the enterprise script form: header byte (0x71 mainnet,
-- 0x70 testnet) followed by the 28-byte script hash, encoded as bech32.
htlcScriptAddress :: Text -> Either Text Text
htlcScriptAddress = scriptAddressFromHash htlcScriptHashHex

-- | Derive the bech32 enterprise-script address for an arbitrary
-- 28-byte script hash on the given network. Generalises
-- 'htlcScriptAddress' so callers (e.g. the HTLC watcher) can compare
-- snapshot UTxO addresses to a known script-hash without re-implementing
-- bech32 encoding.
scriptAddressFromHash :: Text -> Text -> Either Text Text
scriptAddressFromHash hashHex network = do
  scriptHashBytes <- decodeHex28 hashHex
  (header, hrpText) <- case network of
    "Mainnet" -> Right (0x71 :: Int, "addr" :: Text)
    "Preview" -> Right (0x70, "addr_test")
    "Preprod" -> Right (0x70, "addr_test")
    _ -> Left $ "Unknown network: " <> network
  hrp <- case Bech32.humanReadablePartFromText hrpText of
    Left e -> Left $ "Invalid HRP: " <> T.pack (show e)
    Right h -> Right h
  let addrBytes = BS.cons (fromIntegral header) scriptHashBytes
      dataPart = Bech32.dataPartFromBytes addrBytes
  Right $ Bech32.encodeLenient hrp dataPart

-- | Decode a hex string that must yield exactly 28 bytes.
decodeHex28 :: Text -> Either Text ByteString
decodeHex28 s = case Base16.decode (TE.encodeUtf8 s) of
  Left e -> Left $ "Hex decode failed: " <> T.pack e
  Right b
    | BS.length b == 28 -> Right b
    | otherwise -> Left $ "Expected 28 bytes, got " <> T.pack (show (BS.length b))

-- | Extract a 28-byte payment vkey hash from a Cardano address.
--
-- Accepts either:
--   * a 56-char hex string already representing the pkh, or
--   * a bech32 @addr@/@addr_test@ payment address whose payment credential
--     is a vkey hash (header types 0, 2, 4, 6).
--
-- Refuses script-credential addresses (types 1, 3, 5, 7) and stake addresses.
addressOrPkhToBytes :: Text -> Either Text ByteString
addressOrPkhToBytes s
  | T.length s == 56, T.all isHex s = decodeHex28 s
  | otherwise = bech32PaymentPkh s
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

bech32PaymentPkh :: Text -> Either Text ByteString
bech32PaymentPkh addrText = do
  (_hrp, dataPart) <- case Bech32.decodeLenient addrText of
    Left e -> Left $ "Bech32 decode failed: " <> T.pack (show e)
    Right pair -> Right pair
  bytes <- case Bech32.dataPartToBytes dataPart of
    Nothing -> Left "Bech32 data part is not byte-aligned"
    Just b -> Right b
  if BS.length bytes < 29
    then Left $ "Address too short: " <> T.pack (show (BS.length bytes)) <> " bytes"
    else
      let header = BS.head bytes
          headerType = (fromIntegral header `shiftR` 4) .&. 0x0F :: Int
          isPaymentVkey = headerType `elem` [0, 2, 4, 6]
      in if isPaymentVkey
        then Right $ BS.take 28 (BS.drop 1 bytes)
        else Left "Address payment credential is a script, not a vkey"

-- | Hex-encode bytes.
hexEncode :: ByteString -> Text
hexEncode = TE.decodeUtf8 . Base16.encode

-- | CBOR-encode the HTLC datum:
--   @Constr 0 [hash, timeout, sender_pkh, receiver_pkh]@
--
-- Constructor 0 is encoded as Plutus Data CBOR tag 121 (Constr 0..6 →
-- tags 121..127).
mkDatumCbor
  :: ByteString -- ^ 32-byte payment hash
  -> Int64      -- ^ timeout slot
  -> ByteString -- ^ 28-byte sender pkh
  -> ByteString -- ^ 28-byte receiver pkh
  -> ByteString
mkDatumCbor hashB timeout senderB receiverB =
  encodeTerm $
    CBOR.TTagged 121 $ CBOR.TList
      [ CBOR.TBytes hashB
      , CBOR.TInteger (fromIntegral timeout)
      , CBOR.TBytes senderB
      , CBOR.TBytes receiverB
      ]

-- | CBOR-encode the @Claim(preimage)@ redeemer (constructor 0).
mkClaimRedeemerCbor :: ByteString -> ByteString
mkClaimRedeemerCbor preimage =
  encodeTerm $ CBOR.TTagged 121 $ CBOR.TList [CBOR.TBytes preimage]

-- | CBOR-encoded @Refund@ redeemer (constructor 1, no fields).
refundRedeemerCbor :: ByteString
refundRedeemerCbor =
  encodeTerm $ CBOR.TTagged 122 $ CBOR.TList []

encodeTerm :: CBOR.Term -> ByteString
encodeTerm = LBS.toStrict . CBOR.toLazyByteString . CBOR.encodeTerm
