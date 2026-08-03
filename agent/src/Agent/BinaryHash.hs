module Agent.BinaryHash
  ( getBinaryHash
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (getExecutablePath)

-- | Compute the SHA-256 hash of this executable's binary on disk.
-- Returns the hash as @"sha256:<hex>"@.
getBinaryHash :: IO Text
getBinaryHash = do
  path <- getExecutablePath
  contents <- BS.readFile path
  let digest = hash contents :: Digest SHA256
  pure $ "sha256:" <> T.pack (show digest)
