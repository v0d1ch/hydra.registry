module Api.Validation where

import Data.Char (isHexDigit)
import Data.Text (Text)
import Data.Text qualified as T

data AddressFormat
  = Bech32Address
  | HexAddress
  | Base58Address
  deriving stock (Eq, Show)

-- | Validate a Cardano address format.
-- Returns Right with the format if valid, Left with error message if invalid.
validateAddress :: Text -> Either Text AddressFormat
validateAddress addr
  | T.null addr = Left "Empty address"
  | isBech32 addr = Right Bech32Address
  | isHexAddress addr = Right HexAddress
  | isBase58 addr = Right Base58Address
  | otherwise = Left $ "Invalid address format: " <> addr

-- | Check if address is bech32 (addr1..., addr_test1..., stake1..., stake_test1...)
isBech32 :: Text -> Bool
isBech32 addr =
  case bech32Prefix addr of
    Just prefix ->
      T.length addr > T.length prefix
        && T.all isBech32Char (T.drop (T.length prefix) addr)
    Nothing -> False

bech32Prefix :: Text -> Maybe Text
bech32Prefix addr
  | "addr_test1" `T.isPrefixOf` addr = Just "addr_test1"
  | "addr1" `T.isPrefixOf` addr = Just "addr1"
  | "stake_test1" `T.isPrefixOf` addr = Just "stake_test1"
  | "stake1" `T.isPrefixOf` addr = Just "stake1"
  | otherwise = Nothing

isBech32Char :: Char -> Bool
isBech32Char c = c `elem` ("023456789acdefghjklmnpqrstuvwxyz" :: String)

-- | Check if address is hex-encoded (raw credential hash or full address hex)
isHexAddress :: Text -> Bool
isHexAddress addr =
  let len = T.length addr
   in len >= 56
        && len <= 130
        && T.all isHexDigit addr

-- | Check if address is base58 (Byron era)
isBase58 :: Text -> Bool
isBase58 addr =
  let len = T.length addr
   in len >= 20
        && len <= 120
        && T.all isBase58Char addr
 where
  isBase58Char :: Char -> Bool
  isBase58Char c = c `elem` ("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" :: String)
