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

-- | Create invoice request.
-- @receiverOnChainId@ is the 28-byte hex pkh of the receiver's
-- hydra-node @--cardano-signing-key@; the route-finder matches it
-- against @head_participants@ to locate the destination head, and the
-- same key signs the final HTLC claim. The receiver picks where
-- claimed funds land at claim-tx build time.
data CreateInvoiceRequest = CreateInvoiceRequest
  { receiverOnChainId :: Text
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
  , receiverOnChainId :: Text
  , paymentHash :: Text
  , amountLovelace :: Int64
  , memo :: Maybe Text
  , status :: Text
  , expiresAt :: UTCTime
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Find routes request. @senderOnChainId@ identifies the sender's
-- participation in some head (graph lookup); the lock tx for hop 0
-- comes from a UTxO the sender chooses at lock-build time.
data FindRoutesRequest = FindRoutesRequest
  { invoiceId :: Text
  , senderOnChainId :: Text
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
  , senderAddress :: Text
  , receiverAddress :: Text
  , htlcStatus :: Text
  , htlcTxHash :: Maybe Text
  , secretHash :: Text
  , timeoutSlot :: Int64
  , fee :: Int64
  , preimage :: Maybe Text
  , lockedAt :: Maybe UTCTime
  , claimedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Request to submit a revealed preimage
data SubmitPreimageRequest = SubmitPreimageRequest
  { preimage :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Generic success message response
data MessageResponse = MessageResponse
  { message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ─── HTLC tx blueprints ───
--
-- The registry doesn't assemble full Conway-era L2 transactions — that
-- requires the head's protocol parameters (cost models, exec units),
-- which the registry doesn't track. Instead it returns a /blueprint/
-- with every protocol-specific field already computed: the validator
-- address, the HTLC datum CBOR, the redeemer CBOR, validity slots, and
-- the required signer pkh. Callers (typically a bridge agent or a sender
-- client) plug those into a tx body skeleton built by their own
-- hydra-node helpers, then sign and submit via @NewTx@.

-- | The HTLC validator script — same content for every network.
data HtlcValidatorResponse = HtlcValidatorResponse
  { scriptHash :: Text -- ^ 28-byte hex
  , scriptCborHex :: Text -- ^ Plutus V3 validator bytes, hex-encoded
  , scriptType :: Text -- ^ "PlutusV3"
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Decoded view of the HTLC datum, alongside its CBOR encoding.
data HtlcDatumView = HtlcDatumView
  { paymentHash :: Text -- ^ 32-byte hex (matches the invoice)
  , timeoutSlot :: Int64
  , senderPkh :: Text -- ^ 28-byte hex
  , receiverPkh :: Text -- ^ 28-byte hex
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Lock-tx blueprint: everything needed to construct the L2 tx that
-- locks hop @i@ of the route into the HTLC validator. The caller
-- chooses input UTxOs from their head's snapshot, balances the tx, and
-- signs.
--
-- @refScriptUtxo@ is set when the head has a published shared
-- ref-script UTxO ('headRefScriptUtxo' on the row). When set, the lock
-- output omits the inline reference script and the consumer's claim/
-- refund tx spends the script via @--spending-tx-in-reference@. When
-- unset (the bridge agent hasn't published one yet), the consumer must
-- inline the validator on the lock output and source it again at claim
-- time — which the @lockAmountLovelace@ floor is sized for.
data LockTxBlueprint = LockTxBlueprint
  { headId :: Text
  , scriptAddress :: Text -- ^ HTLC validator address (bech32) for the route's network
  , scriptHash :: Text
  , datum :: HtlcDatumView
  , datumCborHex :: Text -- ^ inline this in the HTLC output
  , validatorRefScriptCborHex :: Text -- ^ inline this as the output's @reference_script@ when @refScriptUtxo@ is null
  , refScriptUtxo :: Maybe Text -- ^ @"txhash#ix"@ to use as @--spending-tx-in-reference@ at claim time, when published
  , lockAmountLovelace :: Int64 -- ^ amount + fees of remaining downstream hops
  , validityUpperSlot :: Int64 -- ^ upper bound for tx validity (must be < timeoutSlot)
  , requiredSignerPkh :: Text -- ^ locker's vkey hash (sender of this hop)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Body for the claim endpoint: the receiver supplies the preimage that
-- hashes to the invoice's payment hash.
data ClaimTxRequest = ClaimTxRequest
  { preimage :: Text -- ^ hex-encoded preimage bytes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Claim-tx blueprint.
data ClaimTxBlueprint = ClaimTxBlueprint
  { headId :: Text
  , htlcInputTxHash :: Text -- ^ from @route_hops.htlc_tx_hash@
  , htlcInputIndex :: Int -- ^ output index of the HTLC UTxO inside that tx
  , refScriptUtxo :: Maybe Text -- ^ @"txhash#ix"@ for @--spending-tx-in-reference@; if null, the HTLC UTxO must carry the validator inline
  , redeemerCborHex :: Text -- ^ @Claim(preimage)@ as Plutus Data CBOR
  , validityUpperSlot :: Int64
  , requiredSignerPkh :: Text -- ^ claimer pkh = receiver of this hop
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Refund-tx blueprint.
data RefundTxBlueprint = RefundTxBlueprint
  { headId :: Text
  , htlcInputTxHash :: Text
  , htlcInputIndex :: Int
  , refScriptUtxo :: Maybe Text -- ^ same role as in ClaimTxBlueprint
  , redeemerCborHex :: Text -- ^ @Refund@ as Plutus Data CBOR
  , validityLowerSlot :: Int64 -- ^ lower bound for tx validity (must be > timeoutSlot)
  , requiredSignerPkh :: Text -- ^ refunder pkh = sender of this hop
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Body for the @POST /heads/{id}/ref-script@ endpoint: operator (or
-- bridge agent) tells the registry where they published the head's
-- shared HTLC reference-script UTxO.
data SetRefScriptRequest = SetRefScriptRequest
  { utxo :: Text -- ^ @"txhash#ix"@ format
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
