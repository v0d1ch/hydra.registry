module Api.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Registration request
data RegisterHead = RegisterHead
  { host :: Text
  , port :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Registration response
data RegisterHeadResponse = RegisterHeadResponse
  { headId :: Text
  , status :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Head info for listing
data HeadInfo = HeadInfo
  { headId :: Text
  , host :: Text
  , port :: Int
  , status :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Blockfrost-compatible amount
data Amount = Amount
  { unit :: Text
  , quantity :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Blockfrost-compatible UTxO response
data UtxoResponse = UtxoResponse
  { address :: Text
  , tx_hash :: Text
  , output_index :: Int
  , amount :: [Amount]
  , data_hash :: Maybe Text
  , inline_datum :: Maybe Value
  , reference_script_hash :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Per-head UTxO response (for cross-head queries)
data HeadUtxoResponse = HeadUtxoResponse
  { head_id :: Text
  , head_status :: Text
  , utxos :: [UtxoResponse]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Health check response
data HealthResponse = HealthResponse
  { status :: Text
  , headCount :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Error response
data ErrorResponse = ErrorResponse
  { errorMsg :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ErrorResponse where
  toJSON (ErrorResponse msg) = object ["error" .= msg]

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \v ->
    ErrorResponse <$> v .: "error"
