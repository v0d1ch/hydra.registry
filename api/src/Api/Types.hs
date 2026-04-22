module Api.Types where

import Data.Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Registration request
data RegisterHead = RegisterHead
  { host :: Text
  , port :: Int
  , bridge :: Maybe Bool
  , feeLovelace :: Maybe Int
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

-- | Stats response
data StatsResponse = StatsResponse
  { headCount :: Int
  , totalUtxos :: Int
  , headsByStatus :: Map Text Int
  , explorerHeadCount :: Int
  , uniqueParticipants :: Int
  , headsByNetwork :: Map Text Int
  , totalCommittedLovelace :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Explorer stats response (filterable)
data ExplorerStatsResponse = ExplorerStatsResponse
  { explorerHeadCount :: Int
  , uniqueParticipants :: Int
  , totalCommittedLovelace :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Explorer head info (on-chain data from hydra-explorer)
data ExplorerHeadInfo = ExplorerHeadInfo
  { headId :: Text
  , network :: Text
  , networkMagic :: Int
  , version :: Text
  , status :: Text
  , contestationPeriod :: Maybe Int
  , contestations :: Maybe Int
  , snapshotNumber :: Maybe Int
  , contestationDeadline :: Maybe Text
  , point :: Maybe Value
  , blockNo :: Maybe Int
  , members :: Maybe Value
  , seedTxIn :: Maybe Text
  , firstSeenAt :: UTCTime
  , lastUpdatedAt :: UTCTime
  , registered :: Bool
  , htlcEnabled :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Enriched head detail (registered head + optional explorer data)
data EnrichedHeadDetail = EnrichedHeadDetail
  { headId :: Text
  , host :: Text
  , port :: Int
  , status :: Text
  , utxoCount :: Int
  , registeredAt :: UTCTime
  , lastSeenAt :: Maybe UTCTime
  , onChain :: Maybe ExplorerHeadOnChain
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | On-chain metadata subset merged into head detail
data ExplorerHeadOnChain = ExplorerHeadOnChain
  { network :: Text
  , onChainStatus :: Text
  , contestationPeriod :: Maybe Int
  , contestations :: Maybe Int
  , snapshotNumber :: Maybe Int
  , contestationDeadline :: Maybe Text
  , members :: Maybe Value
  , seedTxIn :: Maybe Text
  , blockNo :: Maybe Int
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

-- ─── Participant types ───

-- | Head info for a participant's address lookup
data ParticipantHeadInfo = ParticipantHeadInfo
  { headId :: Text
  , address :: Text
  , vkey :: Maybe Text
  , onChainId :: Maybe Text
  , committedLovelace :: Int64
  , committedTxRef :: Maybe Text
  , headStatus :: Text
  , network :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Check head response (pre-registration validation)
data CheckHeadResponse = CheckHeadResponse
  { headId :: Text
  , headStatus :: Text
  , alreadyRegistered :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ─── Deposit types ───

-- | Request to create a deposit transaction
data DepositRequest = DepositRequest
  { host :: Text
  , port :: Int
  , network :: Text -- "Preview" | "Preprod" | "Mainnet"
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response containing the patched deposit tx CBOR
data DepositResponse = DepositResponse
  { depositTxCbor :: Text -- Hex-encoded patched deposit tx CBOR
  , message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ─── Relay graph types ───

data SubgraphNode = SubgraphNode
  { headId :: Text
  , network :: Text
  , hasHtlc :: Bool
  , isUserHead :: Bool
  , participants :: [Text]
  , committedLovelace :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SubgraphEdge = SubgraphEdge
  { fromHead :: Text
  , toHead :: Text
  , bridgeAddress :: Text
  , fee :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SubgraphResponse = SubgraphResponse
  { nodes :: [SubgraphNode]
  , edges :: [SubgraphEdge]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ─── Relay types ───

-- | Create invoice request
data CreateInvoiceRequest = CreateInvoiceRequest
  { receiverAddress :: Text
  , paymentHash :: Text
  , amountLovelace :: Int64
  , memo :: Maybe Text
  , expiresInSeconds :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Invoice response
data InvoiceResponse = InvoiceResponse
  { invoiceId :: Text
  , receiverAddress :: Text
  , paymentHash :: Text
  , amountLovelace :: Int64
  , memo :: Maybe Text
  , status :: Text
  , expiresAt :: UTCTime
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Find routes request
data FindRoutesRequest = FindRoutesRequest
  { invoiceId :: Text
  , senderAddress :: Text
  , network :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A single route option
data RouteResponse = RouteResponse
  { routeId :: Text
  , hops :: [RouteHopResponse]
  , totalFee :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A hop in a route response
data RouteHopResponse = RouteHopResponse
  { headId :: Text
  , bridgeAddress :: Text
  , fee :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Payment status response (with per-hop details)
data PaymentStatusResponse = PaymentStatusResponse
  { routeId :: Text
  , invoiceId :: Text
  , senderAddress :: Text
  , receiverAddress :: Text
  , amountLovelace :: Int64
  , status :: Text
  , totalFee :: Int64
  , network :: Text
  , hops :: [HopStatusResponse]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Individual hop status in a payment
data HopStatusResponse = HopStatusResponse
  { hopIndex :: Int
  , headId :: Text
  , bridgeAddress :: Text
  , htlcStatus :: Text
  , htlcTxHash :: Maybe Text
  , secretHash :: Text
  , timeoutSlot :: Int64
  , fee :: Int64
  , lockedAt :: Maybe UTCTime
  , claimedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
