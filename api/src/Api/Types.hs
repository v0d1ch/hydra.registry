module Api.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
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

-- | Detailed head info (single head endpoint)
data HeadDetailResponse = HeadDetailResponse
  { headId :: Text
  , host :: Text
  , port :: Int
  , status :: Text
  , utxoCount :: Int
  , registeredAt :: UTCTime
  , lastSeenAt :: Maybe UTCTime
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

-- | Blockfrost-compatible UTxO response (with extra head_id field)
data UtxoResponse = UtxoResponse
  { address :: Text
  , tx_hash :: Text
  , output_index :: Int
  , amount :: [Amount]
  , data_hash :: Maybe Text
  , inline_datum :: Maybe Value
  , reference_script_hash :: Maybe Text
  , head_id :: Text
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

-- | Aggregated balance response
data BalanceResponse = BalanceResponse
  { address :: Text
  , headId :: Text
  , lovelace :: Text
  , tokens :: [Amount]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Health check response
data HealthResponse = HealthResponse
  { status :: Text
  , headCount :: Int
  , dbConnected :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Root endpoint response
data RootResponse = RootResponse
  { apiVersion :: Text
  , description :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Yoroi-compatible request for UTxOs by addresses
data YoroiUtxoRequest = YoroiUtxoRequest
  { addresses :: [Text]
  , page :: Maybe Int
  , pageSize :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Yoroi-compatible asset in UTxO response
data YoroiAsset = YoroiAsset
  { assetId :: Text
  , policyId :: Text
  , name :: Text
  , amount :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Yoroi-compatible UTxO response
data YoroiUtxoResponse = YoroiUtxoResponse
  { utxo_id :: Text
  , tx_hash :: Text
  , tx_index :: Int
  , block_num :: Int
  , receiver :: Text
  , amount :: Text
  , dataHash :: Maybe Text
  , assets :: [YoroiAsset]
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
