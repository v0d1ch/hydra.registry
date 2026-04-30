module Api where

import Api.Types
import Api.Validation (validateAddress)
import Cache (Cache, insertCache, lookupCache)
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Db qualified
import Db.Schema (ExplorerHead (..), Head (..), HeadParticipant (..), Invoice (..), PaymentRoute (..), RouteHop (..), Utxo (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraEvent (..), validateHydraNode)
import Hydra.Htlc qualified as Htlc
import Indexer qualified
import Logging (Logger)
import Metrics (Metrics, renderMetrics)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..)
  , cors
  , simpleHeaders
  , simpleMethods
  )
import Relay.Graph qualified as Graph
import Relay.Slot qualified as Slot
import Servant

-- | Our own endpoints that live under /api/v1/
type ApiV1Routes =
  "health" :> Get '[JSON] HealthResponse
    :<|> "heads" :> "check" :> QueryParam "host" Text :> QueryParam "port" Int :> Get '[JSON] CheckHeadResponse
    :<|> "heads" :> "register" :> ReqBody '[JSON] RegisterHead :> Post '[JSON] RegisterHeadResponse
    :<|> "heads" :> QueryParam "count" Int :> QueryParam "page" Int :> Get '[JSON] [HeadInfo]
    :<|> "heads" :> Capture "headId" Text :> Get '[JSON] EnrichedHeadDetail
    :<|> "heads" :> Capture "headId" Text :> "addresses" :> Get '[JSON] [Text]
    :<|> "heads" :> Capture "headId" Text :> "addresses" :> Capture "address" Text :> "balance" :> Get '[JSON] BalanceResponse
    :<|> "heads" :> Capture "headId" Text :> "addresses" :> Capture "address" Text :> "utxos" :> Get '[JSON] [UtxoResponse]
    :<|> "admin" :> "heads" :> Capture "headId" Text :> Delete '[JSON] NoContent
    :<|> "metrics" :> Get '[PlainText] Text
    :<|> "stats" :> Get '[JSON] StatsResponse
    :<|> "explorer" :> "heads" :> QueryParam "count" Int :> QueryParam "page" Int :> QueryParam "status" Text :> QueryParam "network" Text :> Get '[JSON] [ExplorerHeadInfo]
    :<|> "explorer" :> "heads" :> Capture "headId" Text :> Get '[JSON] ExplorerHeadInfo
    :<|> "explorer" :> "heads" :> Capture "headId" Text :> "participants" :> Get '[JSON] [ParticipantHeadInfo]
    :<|> "explorer" :> "stats" :> QueryParam "status" Text :> QueryParam "network" Text :> Get '[JSON] ExplorerStatsResponse
    -- Participant lookup
    :<|> "addresses" :> Capture "address" Text :> "heads" :> Get '[JSON] [ParticipantHeadInfo]
    -- Relay endpoints
    :<|> "relay" :> "graph" :> QueryParam "network" Text :> Get '[JSON] SubgraphResponse
    :<|> "relay" :> "invoices" :> ReqBody '[JSON] CreateInvoiceRequest :> Post '[JSON] InvoiceResponse
    :<|> "relay" :> "invoices" :> Capture "invoiceId" Text :> Get '[JSON] InvoiceResponse
    :<|> "relay" :> "routes" :> ReqBody '[JSON] FindRoutesRequest :> Post '[JSON] [RouteResponse]
    :<|> "relay" :> "routes" :> Capture "routeId" Text :> "execute" :> Post '[JSON] PaymentStatusResponse
    :<|> "relay" :> "payments" :> Capture "paymentId" Text :> Get '[JSON] PaymentStatusResponse
    :<|> "relay" :> "preimage" :> Capture "paymentHash" Text :> ReqBody '[JSON] SubmitPreimageRequest :> Post '[JSON] MessageResponse
    -- HTLC tx blueprints
    :<|> "htlc" :> "validator" :> Get '[JSON] HtlcValidatorResponse
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "lock-tx" :> Post '[JSON] LockTxBlueprint
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "claim-tx" :> ReqBody '[JSON] ClaimTxRequest :> Post '[JSON] ClaimTxBlueprint
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "refund-tx" :> Post '[JSON] RefundTxBlueprint

-- | Full API type
type API =
  -- GET / — root endpoint with version and docs link
  Get '[JSON] RootResponse
    -- /api/v1/* — our API endpoints
    :<|> "api" :> "v1" :> ApiV1Routes
    -- /addresses/:address/utxos — Blockfrost-compatible (wallet compat, root level)
    :<|> "addresses" :> Capture "address" Text :> "utxos"
          :> QueryParam "count" Int :> QueryParam "page" Int :> QueryParam "order" Text
          :> Get '[JSON] [UtxoResponse]
    -- /api/txs/utxoForAddresses — Yoroi-compatible (wallet compat, current path)
    :<|> "api" :> "txs" :> "utxoForAddresses" :> ReqBody '[JSON] YoroiUtxoRequest :> Post '[JSON] [YoroiUtxoResponse]
    -- Static file serving (catch-all for website)
    :<|> Raw

api :: Proxy API
api = Proxy

-- | Application environment shared across handlers
data AppEnv = AppEnv
  { pool :: Pool
  , eventQueue :: TQueue HydraEvent
  , logger :: Logger
  , metrics :: Metrics
  , addressCache :: Cache [UtxoResponse]
  , staticDir :: FilePath
  , relayGraph :: TVar Graph.RelayGraph
  , -- | Highest L1 chain slot seen across any registered head's
    -- Greetings. Bumped by the Indexer; read by handlers that need to
    -- derive timeouts/validity bounds from chain time rather than the
    -- registry's local system clock.
    latestChainSlot :: TVar Int64
  , htlcScriptHash :: Maybe Text
  , htlcScriptCbor :: Maybe Text
  }

-- | Create the Servant server
server :: AppEnv -> Server API
server env =
  handleRoot
    :<|> apiV1Server env
    :<|> handleAddressUtxos env.pool env.addressCache
    :<|> handleYoroiUtxos env.pool
    :<|> serveDirectoryWebApp env.staticDir

-- | Server for /api/v1/* routes
apiV1Server :: AppEnv -> Server ApiV1Routes
apiV1Server env =
  handleHealth env.pool
    :<|> handleCheckHead env.logger env.pool
    :<|> handleRegister env.logger env.pool env.eventQueue
    :<|> handleListHeads env.pool
    :<|> handleHeadDetail env.pool
    :<|> handleHeadAddresses env.pool
    :<|> handleAddressBalance env.pool
    :<|> handleHeadUtxos env.pool
    :<|> handleAdminDeleteHead env.pool
    :<|> handleMetrics env.metrics
    :<|> handleStats env.pool
    :<|> handleListExplorerHeads env.pool env.htlcScriptHash
    :<|> handleExplorerHeadDetail env.pool env.htlcScriptHash
    :<|> handleExplorerHeadParticipants env.pool
    :<|> handleExplorerStats env.pool
    :<|> handleAddressHeads env.pool
    :<|> handleRelayGraph env.pool env.htlcScriptHash
    :<|> handleCreateInvoice env.pool
    :<|> handleGetInvoice env.pool
    :<|> handleFindRoutes env.pool env.relayGraph env.latestChainSlot
    :<|> handleExecuteRoute env.pool
    :<|> handleGetPayment env.pool
    :<|> handleSubmitPreimage env.pool
    :<|> handleHtlcValidator env.htlcScriptCbor
    :<|> handleLockTx env.pool env.latestChainSlot
    :<|> handleClaimTx env.pool env.latestChainSlot
    :<|> handleRefundTx env.pool env.latestChainSlot

-- | CORS middleware that allows the frontend to talk to the API
corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just policy
 where
  policy =
    CorsResourcePolicy
      { corsOrigins = Nothing -- allow all origins
      , corsMethods = simpleMethods <> ["DELETE", "PUT", "PATCH"]
      , corsRequestHeaders = simpleHeaders <> ["Content-Type", "Authorization"]
      , corsExposedHeaders = Nothing
      , corsMaxAge = Just 86400 -- cache preflight for 24h
      , corsVaryOrigin = True
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }

-- | GET /
handleRoot :: Handler RootResponse
handleRoot =
  pure
    RootResponse
      { apiVersion = "0.1.0"
      , description = "Hydra Registry API — query L2 UTxO state across Hydra heads"
      }

-- | GET /api/v1/health
handleHealth :: Pool -> Handler HealthResponse
handleHealth pool = do
  heads <- liftIO $ Db.getAllHeads pool
  dbOk <- liftIO $ Db.checkDbConnectivity pool
  pure $
    HealthResponse
      { status = if dbOk then "ok" else "degraded"
      , headCount = length heads
      , dbConnected = dbOk
      }

-- | GET /api/v1/heads/check?host=...&port=...
handleCheckHead :: Logger -> Pool -> Maybe Text -> Maybe Int -> Handler CheckHeadResponse
handleCheckHead logger pool mHost mPort = do
  hostAddr <- maybe (throwError $ err400{errBody = Aeson.encode $ ErrorResponse "host is required"}) pure mHost
  portNum <- maybe (throwError $ err400{errBody = Aeson.encode $ ErrorResponse "port is required"}) pure mPort
  result <- liftIO $ validateHydraNode logger hostAddr portNum
  case result of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right HeadGreetings{greeterHeadId, greeterHeadStatus} -> do
      existing <- liftIO $ Db.getHead pool greeterHeadId
      pure $ CheckHeadResponse greeterHeadId greeterHeadStatus (maybe False (const True) existing)
    Right _ ->
      throwError $ err500{errBody = Aeson.encode $ ErrorResponse "Unexpected response from Hydra node"}

-- | POST /api/v1/heads/register
handleRegister :: Logger -> Pool -> TQueue HydraEvent -> RegisterHead -> Handler RegisterHeadResponse
handleRegister logger pool eventQueue req = do
  result <- liftIO $ Indexer.registerHead logger pool eventQueue req.host req.port
  case result of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right HeadGreetings{greeterHeadId} ->
      pure $ RegisterHeadResponse greeterHeadId "connected"
    Right _ ->
      throwError $ err500{errBody = Aeson.encode $ ErrorResponse "Unexpected response"}

-- | GET /api/v1/heads (with optional pagination)
handleListHeads :: Pool -> Maybe Int -> Maybe Int -> Handler [HeadInfo]
handleListHeads pool mCount mPage = do
  let count = min 100 $ maybe 100 (max 1) mCount
      page = maybe 1 (max 1) mPage
  heads <- liftIO $ Db.getAllHeadsPaginated pool count page
  pure $ map toHeadInfo heads
 where
  toHeadInfo h =
    HeadInfo
      { headId = h.headId
      , host = h.headHost
      , port = fromIntegral h.headPort
      , status = h.headStatus
      }

-- | GET /api/v1/heads/:headId (enriched with explorer data when available)
handleHeadDetail :: Pool -> Text -> Handler EnrichedHeadDetail
handleHeadDetail pool hid = do
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just h -> do
      utxoCount <- liftIO $ Db.countUtxosForHead pool hid
      mExplorer <- liftIO $ Db.getExplorerHead pool hid
      pure
        EnrichedHeadDetail
          { headId = h.headId
          , host = h.headHost
          , port = fromIntegral h.headPort
          , status = h.headStatus
          , utxoCount = utxoCount
          , registeredAt = h.createdAt
          , lastSeenAt = h.lastMessageAt
          , onChain = explorerHeadToOnChain <$> mExplorer
          }

-- | GET /api/v1/heads/:headId/addresses
handleHeadAddresses :: Pool -> Text -> Handler [Text]
handleHeadAddresses pool hid = do
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ ->
      liftIO $ Db.getAddressesForHead pool hid

-- | GET /api/v1/heads/:headId/addresses/:address/balance
handleAddressBalance :: Pool -> Text -> Text -> Handler BalanceResponse
handleAddressBalance pool hid addr = do
  case validateAddress addr of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right _ -> pure ()
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ -> do
      (totalLovelace, assetMap) <- liftIO $ Db.getBalanceForAddressInHead pool hid addr
      pure
        BalanceResponse
          { address = addr
          , headId = hid
          , lovelace = T.pack $ show totalLovelace
          , tokens = assetMapToAmounts assetMap
          }

-- | GET /api/v1/heads/:headId/addresses/:address/utxos
handleHeadUtxos :: Pool -> Text -> Text -> Handler [UtxoResponse]
handleHeadUtxos pool hid addr = do
  case validateAddress addr of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right _ -> pure ()
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ -> do
      utxos <- liftIO $ Db.getUtxosByAddressAndHead pool hid addr
      pure $ map (utxoToResponse hid) utxos

-- | GET /addresses/:address/utxos (Blockfrost-compatible flat response with pagination)
handleAddressUtxos :: Pool -> Cache [UtxoResponse] -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Handler [UtxoResponse]
handleAddressUtxos pool cache addr mCount mPage _mOrder = do
  case validateAddress addr of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right _ -> pure ()
  let count = min 100 $ maybe 100 (max 1) mCount
      page = maybe 1 (max 1) mPage
  -- Check cache only for default (first page)
  let useCache = page == 1 && count == 100
  if useCache
    then do
      cached <- liftIO $ lookupCache cache addr
      case cached of
        Just result -> pure result
        Nothing -> do
          response <- fetchUtxos count page
          liftIO $ insertCache cache addr response
          pure response
    else fetchUtxos count page
 where
  fetchUtxos count page = do
    utxos <- liftIO $ Db.getUtxosByAddressFlat pool addr count page
    pure $ map (\u -> utxoToResponse u.utxoHeadId u) utxos

-- | POST /api/txs/utxoForAddresses (Yoroi-compatible)
handleYoroiUtxos :: Pool -> YoroiUtxoRequest -> Handler [YoroiUtxoResponse]
handleYoroiUtxos pool req = do
  let addrs = req.addresses
  when (null addrs) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "addresses must not be empty"}
  when (length addrs > 50) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "max 50 addresses"}
  -- Validate all addresses
  mapM_
    ( \addr -> case validateAddress addr of
        Left err -> throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
        Right _ -> pure ()
    )
    addrs
  let page = maybe 1 (max 1) req.page
      pageSize = min 100 $ maybe 100 (max 1) req.pageSize
  results <- liftIO $ Db.getUtxosByAddressesWithSnapshot pool addrs pageSize page
  pure $ map (uncurry utxoToYoroiResponse) results

-- | Convert a DB UTxO to Yoroi-compatible response format
utxoToYoroiResponse :: Utxo Identity -> Int32 -> YoroiUtxoResponse
utxoToYoroiResponse u snapNum =
  YoroiUtxoResponse
    { utxo_id = u.utxoTxHash <> ":" <> T.pack (show u.utxoOutputIndex)
    , tx_hash = u.utxoTxHash
    , tx_index = fromIntegral u.utxoOutputIndex
    , block_num = fromIntegral snapNum
    , receiver = u.utxoAddress
    , amount = T.pack $ show u.utxoLovelace
    , dataHash = u.utxoDatumHash
    , assets = nativeTokenAssets
    }
 where
  nativeTokenAssets = case u.utxoAssets of
    Aeson.Object obj ->
      [ YoroiAsset
          { assetId = Key.toText policyKey <> "." <> Key.toText assetKey
          , policyId = Key.toText policyKey
          , name = Key.toText assetKey
          , amount = T.pack $ show (round n :: Integer)
          }
      | (policyKey, Aeson.Object assets) <- KM.toList obj
      , (assetKey, Aeson.Number n) <- KM.toList assets
      ]
    _ -> []

-- | DELETE /api/v1/admin/heads/:headId
handleAdminDeleteHead :: Pool -> Text -> Handler NoContent
handleAdminDeleteHead pool hid = do
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ -> do
      liftIO $ Db.deleteHead pool hid
      pure NoContent

-- | GET /api/v1/metrics
handleMetrics :: Metrics -> Handler Text
handleMetrics = liftIO . renderMetrics

-- | GET /api/v1/stats
handleStats :: Pool -> Handler StatsResponse
handleStats pool = do
  (hCount, uCount, byStatus) <- liftIO $ Db.getStats pool
  explorerCount <- liftIO $ Db.countExplorerHeads pool
  (uniqueParticipants', networkCounts, totalCommitted) <- liftIO $ Db.getExplorerStats pool
  pure
    StatsResponse
      { headCount = hCount
      , totalUtxos = uCount
      , headsByStatus = byStatus
      , explorerHeadCount = explorerCount
      , uniqueParticipants = uniqueParticipants'
      , headsByNetwork = networkCounts
      , totalCommittedLovelace = totalCommitted
      }

-- | Convert a DB UTxO row to the Blockfrost-compatible response format
utxoToResponse :: Text -> Utxo Identity -> UtxoResponse
utxoToResponse hid u =
  UtxoResponse
    { address = u.utxoAddress
    , tx_hash = u.utxoTxHash
    , output_index = fromIntegral u.utxoOutputIndex
    , amount = lovelaceAmount : nativeTokenAmounts
    , data_hash = u.utxoDatumHash
    , inline_datum = u.utxoInlineDatum
    , reference_script_hash = u.utxoReferenceScriptHash
    , head_id = hid
    }
 where
  lovelaceAmount =
    Amount
      { unit = "lovelace"
      , quantity = T.pack $ show u.utxoLovelace
      }

  nativeTokenAmounts = case u.utxoAssets of
    Aeson.Object obj ->
      [ Amount
          { unit = Key.toText policyKey <> Key.toText assetKey
          , quantity = T.pack $ show (round n :: Integer)
          }
      | (policyKey, Aeson.Object assets) <- KM.toList obj
      , (assetKey, Aeson.Number n) <- KM.toList assets
      ]
    _ -> []

-- | Convert asset map to list of Amount
assetMapToAmounts :: Map.Map Text (Map.Map Text Integer) -> [Amount]
assetMapToAmounts assets =
  [ Amount
      { unit = policyId <> assetName
      , quantity = T.pack $ show qty
      }
  | (policyId, assetMap) <- Map.toList assets
  , (assetName, qty) <- Map.toList assetMap
  ]

-- ─── Explorer head handlers ───

-- | GET /api/v1/explorer/heads
handleListExplorerHeads :: Pool -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler [ExplorerHeadInfo]
handleListExplorerHeads pool mHtlcHash mCount mPage mStatus mNetwork = do
  let count = min 100 $ maybe 100 (max 1) mCount
      page = maybe 1 (max 1) mPage
  explorerHeads <- liftIO $ Db.getExplorerHeadsPaginated pool count page mStatus mNetwork
  registeredHeads <- liftIO $ Db.getAllHeads pool
  htlcIds <- liftIO $ getHtlcHeadIds pool mHtlcHash
  let registeredIds = Map.fromList [(h.headId, ()) | h <- registeredHeads]
  pure $ map (explorerHeadToInfo registeredIds htlcIds) explorerHeads

-- | GET /api/v1/explorer/heads/:headId
handleExplorerHeadDetail :: Pool -> Maybe Text -> Text -> Handler ExplorerHeadInfo
handleExplorerHeadDetail pool mHtlcHash hid = do
  mExplorer <- liftIO $ Db.getExplorerHead pool hid
  case mExplorer of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Explorer head not found"}
    Just eh -> do
      mRegistered <- liftIO $ Db.getHead pool hid
      htlcIds <- liftIO $ getHtlcHeadIds pool mHtlcHash
      let regIds = maybe Map.empty (\h -> Map.singleton h.headId ()) mRegistered
      pure $ explorerHeadToInfo regIds htlcIds eh

-- | GET /api/v1/explorer/heads/:headId/participants
handleExplorerHeadParticipants :: Pool -> Text -> Handler [ParticipantHeadInfo]
handleExplorerHeadParticipants pool hid = do
  participants <- liftIO $ Db.getParticipantsForHead pool hid
  mExplorer <- liftIO $ Db.getExplorerHead pool hid
  let net = maybe "" (\eh -> eh.explorerNetwork) mExplorer
      status = maybe "Unknown" (\eh -> eh.explorerStatus) mExplorer
  pure
    [ ParticipantHeadInfo
        { headId = hid
        , address = p.participantAddress
        , vkey = p.participantVkey
        , onChainId = p.participantOnChainId
        , committedLovelace = fromIntegral p.participantCommittedLovelace
        , committedTxRef = p.participantCommittedTxRef
        , headStatus = status
        , network = net
        }
    | p <- participants
    ]

-- | GET /api/v1/explorer/stats
handleExplorerStats :: Pool -> Maybe Text -> Maybe Text -> Handler ExplorerStatsResponse
handleExplorerStats pool mStatus mNetwork = do
  (uniqueCount, networkCounts, totalCommitted) <- liftIO $ Db.getFilteredExplorerStats pool mStatus mNetwork
  let headCount = Prelude.sum $ Map.elems networkCounts
  pure
    ExplorerStatsResponse
      { explorerHeadCount = headCount
      , uniqueParticipants = uniqueCount
      , totalCommittedLovelace = totalCommitted
      }

-- | Get the set of head IDs that contain the HTLC script
getHtlcHeadIds :: Pool -> Maybe Text -> IO (Set.Set Text)
getHtlcHeadIds _ Nothing = pure Set.empty
getHtlcHeadIds pool (Just scriptHash) = Set.fromList <$> Db.getHeadsWithScript pool scriptHash

-- | Convert an ExplorerHead DB row to API response
explorerHeadToInfo :: Map.Map Text () -> Set.Set Text -> ExplorerHead Identity -> ExplorerHeadInfo
explorerHeadToInfo registeredIds htlcIds eh =
  ExplorerHeadInfo
    { headId = eh.explorerHeadId
    , network = eh.explorerNetwork
    , networkMagic = fromIntegral eh.explorerNetworkMagic
    , version = eh.explorerVersion
    , status = eh.explorerStatus
    , contestationPeriod = fromIntegral <$> eh.explorerContestationPeriod
    , contestations = fromIntegral <$> eh.explorerContestations
    , snapshotNumber = fromIntegral <$> eh.explorerSnapshotNumber
    , contestationDeadline = eh.explorerContestationDeadline
    , point = eh.explorerPoint
    , blockNo = fromIntegral <$> eh.explorerBlockNo
    , members = eh.explorerMembers
    , seedTxIn = eh.explorerSeedTxIn
    , firstSeenAt = eh.explorerFirstSeenAt
    , lastUpdatedAt = eh.explorerLastUpdatedAt
    , registered = Map.member eh.explorerHeadId registeredIds
    , htlcEnabled = Set.member eh.explorerHeadId htlcIds
    }

-- | Convert an ExplorerHead to the on-chain metadata subset
explorerHeadToOnChain :: ExplorerHead Identity -> ExplorerHeadOnChain
explorerHeadToOnChain eh =
  ExplorerHeadOnChain
    { network = eh.explorerNetwork
    , onChainStatus = eh.explorerStatus
    , contestationPeriod = fromIntegral <$> eh.explorerContestationPeriod
    , contestations = fromIntegral <$> eh.explorerContestations
    , snapshotNumber = fromIntegral <$> eh.explorerSnapshotNumber
    , contestationDeadline = eh.explorerContestationDeadline
    , members = eh.explorerMembers
    , seedTxIn = eh.explorerSeedTxIn
    , blockNo = fromIntegral <$> eh.explorerBlockNo
    }

-- ─── Participant handlers ───

-- | GET /api/v1/addresses/:address/heads
handleAddressHeads :: Pool -> Text -> Handler [ParticipantHeadInfo]
handleAddressHeads pool addr = do
  case validateAddress addr of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right _ -> pure ()
  participants <- liftIO $ Db.getHeadsByParticipantAddress pool addr
  -- Enrich with explorer head data for status/network
  mapM (enrichParticipant pool) participants

enrichParticipant :: Pool -> HeadParticipant Identity -> Handler ParticipantHeadInfo
enrichParticipant pool p = do
  mExplorer <- liftIO $ Db.getExplorerHead pool p.participantHeadId
  let (status', network') = case mExplorer of
        Just eh -> (eh.explorerStatus, eh.explorerNetwork)
        Nothing -> ("unknown", "unknown")
  pure
    ParticipantHeadInfo
      { headId = p.participantHeadId
      , address = p.participantAddress
      , vkey = p.participantVkey
      , onChainId = p.participantOnChainId
      , committedLovelace = p.participantCommittedLovelace
      , committedTxRef = p.participantCommittedTxRef
      , headStatus = status'
      , network = network'
      }

-- ─── Relay handlers ───

-- | GET /api/v1/relay/graph?network=...
-- Returns explorer heads that have participants and edges between heads sharing a participant.
-- Deduplicates edges and caps output to keep responses fast.
handleRelayGraph :: Pool -> Maybe Text -> Maybe Text -> Handler SubgraphResponse
handleRelayGraph pool mHtlcHash mNetwork = do
  network' <- maybe (throwError $ err400{errBody = Aeson.encode $ ErrorResponse "network is required"}) pure mNetwork
  explorerHeads <- liftIO $ Db.getAllExplorerHeads pool
  participants <- liftIO $ Db.getAllParticipants pool
  htlcIds <- liftIO $ getHtlcHeadIds pool mHtlcHash
  -- Locally-registered heads with WS status=Open are eligible to participate
  -- in the graph even when the public hydra-explorer hasn't indexed them yet.
  -- We trust the user's network selection (the @network@ query param) for
  -- these — there's no way to derive the network from a hydra-node WS
  -- Greetings message.
  registeredHeads <- liftIO $ Db.getAllHeads pool
  let registeredOpenIds = Set.fromList [h.headId | h <- registeredHeads, h.headStatus == "Open"]
      -- Filter heads by network
      networkHeads = [eh | eh <- explorerHeads, eh.explorerNetwork == network', eh.explorerStatus == "Open"]
      explorerOpenIds = Set.fromList [eh.explorerHeadId | eh <- networkHeads]
      -- A head qualifies if EITHER the explorer sees it Open in the
      -- requested network OR we have it locally registered as Open.
      headIds = explorerOpenIds `Set.union` registeredOpenIds
      -- Build address→heads map from participants (only small groups to avoid explosion)
      addrToHeads :: Map.Map Text (Set.Set Text)
      addrToHeads =
        Map.filter (\s -> Set.size s >= 2 && Set.size s <= 10) $
          Map.fromListWith Set.union
            [ (p.participantAddress, Set.singleton p.participantHeadId)
            | p <- participants
            , Set.member p.participantHeadId headIds
            ]
      -- Deduplicated edges: one per (from, to) pair, keep first bridge address seen
      edgeMap :: Map.Map (Text, Text) SubgraphEdge
      edgeMap =
        Map.fromList
          [ ( (h1, h2)
            , SubgraphEdge
                { fromHead = h1
                , toHead = h2
                , bridgeAddress = addr
                , fee = 0
                }
            )
          | (addr, hset) <- Map.toList addrToHeads
          , h1 <- Set.toList hset
          , h2 <- Set.toList hset
          , h1 /= h2
          ]
      dedupedEdges = take 500 $ Map.elems edgeMap
      -- Only include nodes that appear in at least one edge
      connectedIds =
        Set.fromList $
          concatMap (\e -> [e.fromHead, e.toHead]) dedupedEdges
      -- Build head→participants and head→committed lovelace maps
      headParticipants :: Map.Map Text [Text]
      headParticipants =
        Map.fromListWith (<>)
          [ (p.participantHeadId, [p.participantAddress])
          | p <- participants
          , Set.member p.participantHeadId connectedIds
          ]
      headLovelace :: Map.Map Text Int64
      headLovelace =
        Map.fromListWith (+)
          [ (p.participantHeadId, p.participantCommittedLovelace)
          | p <- participants
          , Set.member p.participantHeadId connectedIds
          ]
  let explorerNodeIds = Set.fromList [eh.explorerHeadId | eh <- networkHeads]
      explorerNodes =
        [ SubgraphNode
            { headId = eh.explorerHeadId
            , network = eh.explorerNetwork
            , hasHtlc = Set.member eh.explorerHeadId htlcIds
            , isUserHead = False
            , participants = Map.findWithDefault [] eh.explorerHeadId headParticipants
            , committedLovelace = Map.findWithDefault 0 eh.explorerHeadId headLovelace
            }
        | eh <- networkHeads
        , Set.member eh.explorerHeadId connectedIds
        ]
      -- For registered-only heads (not yet seen by the public explorer),
      -- synthesize a node entry. Network = requested filter (we have no
      -- better signal); isUserHead = True so the UI can highlight them.
      registeredOnlyNodes =
        [ SubgraphNode
            { headId = h.headId
            , network = network'
            , hasHtlc = Set.member h.headId htlcIds
            , isUserHead = True
            , participants = Map.findWithDefault [] h.headId headParticipants
            , committedLovelace = Map.findWithDefault 0 h.headId headLovelace
            }
        | h <- registeredHeads
        , h.headStatus == "Open"
        , Set.member h.headId connectedIds
        , not (Set.member h.headId explorerNodeIds)
        ]
  pure
    SubgraphResponse
      { nodes = explorerNodes <> registeredOnlyNodes
      , edges = dedupedEdges
      }

-- | POST /api/v1/relay/invoices
handleCreateInvoice :: Pool -> CreateInvoiceRequest -> Handler InvoiceResponse
handleCreateInvoice pool req = do
  when (T.length req.receiverOnChainId /= 56 || not (T.all isHex req.receiverOnChainId)) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "receiverOnChainId must be 56 hex chars (28-byte vkey hash)"}
  when (req.amountLovelace <= 0) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "amount must be positive"}
  when (T.null req.paymentHash) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "paymentHash is required"}
  now <- liftIO getCurrentTime
  iid <- liftIO $ UUID.toText <$> UUID.nextRandom
  let expirySeconds = maybe 3600 id req.expiresInSeconds
      expiresAt = addUTCTime (fromIntegral expirySeconds) now
  liftIO $ Db.insertInvoice pool iid req.receiverOnChainId req.paymentHash req.amountLovelace req.memo "pending" expiresAt
  pure
    InvoiceResponse
      { invoiceId = iid
      , receiverOnChainId = req.receiverOnChainId
      , paymentHash = req.paymentHash
      , amountLovelace = req.amountLovelace
      , memo = req.memo
      , status = "pending"
      , expiresAt = expiresAt
      , createdAt = now
      }
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | GET /api/v1/relay/invoices/:invoiceId
handleGetInvoice :: Pool -> Text -> Handler InvoiceResponse
handleGetInvoice pool iid = do
  mInvoice <- liftIO $ Db.getInvoice pool iid
  case mInvoice of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Invoice not found"}
    Just inv ->
      pure
        InvoiceResponse
          { invoiceId = inv.invoiceId
          , receiverOnChainId = inv.invoiceReceiverOnChainId
          , paymentHash = inv.invoicePaymentHash
          , amountLovelace = inv.invoiceAmountLovelace
          , memo = inv.invoiceMemo
          , status = inv.invoiceStatus
          , expiresAt = inv.invoiceExpiresAt
          , createdAt = inv.invoiceCreatedAt
          }

-- | POST /api/v1/relay/routes
handleFindRoutes :: Pool -> TVar Graph.RelayGraph -> TVar Int64 -> FindRoutesRequest -> Handler [RouteResponse]
handleFindRoutes pool graphVar chainSlotVar req = do
  -- Look up the invoice
  mInvoice <- liftIO $ Db.getInvoice pool req.invoiceId
  case mInvoice of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Invoice not found"}
    Just inv -> do
      graph <- liftIO $ readTVarIO graphVar
      chainSlot <- liftIO $ readTVarIO chainSlotVar
      -- Routing keys off Cardano key hashes (= hydra-node OnChainIds):
      -- sender's identity from the request, receiver's from the invoice.
      -- The receiver picks where claimed funds ultimately land at
      -- claim-tx build time.
      let routes = Graph.findRoutes graph req.senderOnChainId inv.invoiceReceiverOnChainId req.network 3
      mapM (routeToResponse pool chainSlot req.invoiceId req.senderOnChainId inv.invoiceReceiverOnChainId inv.invoiceAmountLovelace inv.invoicePaymentHash inv.invoiceExpiresAt req.network) routes

routeToResponse :: Pool -> Int64 -> Text -> Text -> Text -> Int64 -> Text -> UTCTime -> Text -> Graph.Route -> Handler RouteResponse
routeToResponse pool chainSlot invoiceId senderAddr receiverAddr amount paymentHash expiresAt network route = do
  rid <- liftIO $ UUID.toText <$> UUID.nextRandom
  -- Expand the dijkstra path into one HTLC per head along the route.
  -- See Relay.Graph.expandRouteToHtlcs for the contract.
  let htlcs = Graph.expandRouteToHtlcs senderAddr receiverAddr route
      hopResponses =
        [ RouteHopResponse
            { headId = h.htlcHopHeadId
            , bridgeAddress = h.htlcHopReceiver
            , fee = h.htlcHopFee
            }
        | h <- htlcs
        ]
  -- Persist the route
  liftIO $
    Db.insertPaymentRoute
      pool
      rid
      invoiceId
      senderAddr
      receiverAddr
      amount
      "requested"
      (Aeson.toJSON hopResponses)
      route.routeTotalFee
      network
  -- Derive timeout slot from chain tip + remaining seconds on the
  -- invoice's @expiresAt@. Falling back to a system-clock conversion
  -- when no chain tip is known yet (e.g. very fresh process before any
  -- Greetings landed). Chain-tip-based math avoids the failure mode
  -- where the registry's local system clock drifts behind chain time
  -- and produces deadlines already in the past from the head's view.
  now <- liftIO getCurrentTime
  let secondsRemaining = max 60 (round (diffUTCTime expiresAt now)) :: Int64
  timeoutSlot <-
    if chainSlot > 0
      then pure (chainSlot + secondsRemaining)
      else case Slot.utcTimeToSlot network expiresAt of
        Just slot -> pure slot
        Nothing ->
          throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ "Unsupported network for slot conversion: " <> network}
  hopRows <- liftIO $
    mapM
      ( \(i, h) -> do
          hid <- UUID.toText <$> UUID.nextRandom
          pure
            ( hid
            , rid
            , i
            , h.htlcHopHeadId
            , h.htlcHopReceiver
            , h.htlcHopSender
            , h.htlcHopReceiver
            , "pending"
            , paymentHash
            , timeoutSlot
            , h.htlcHopFee
            )
      )
      (zip [0 ..] htlcs)
  liftIO $ Db.insertRouteHops pool hopRows
  pure
    RouteResponse
      { routeId = rid
      , hops = hopResponses
      , totalFee = route.routeTotalFee
      }

-- | POST /api/v1/relay/routes/:routeId/execute
handleExecuteRoute :: Pool -> Text -> Handler PaymentStatusResponse
handleExecuteRoute pool rid = do
  mRoute <- liftIO $ Db.getPaymentRoute pool rid
  case mRoute of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Route not found"}
    Just route -> do
      liftIO $ Db.updateRouteStatus pool rid "in_progress"
      buildPaymentStatus pool route{routeStatus = "in_progress"}

-- | GET /api/v1/relay/payments/:paymentId
handleGetPayment :: Pool -> Text -> Handler PaymentStatusResponse
handleGetPayment pool rid = do
  mRoute <- liftIO $ Db.getPaymentRoute pool rid
  case mRoute of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Payment not found"}
    Just route ->
      buildPaymentStatus pool route

buildPaymentStatus :: Pool -> PaymentRoute Identity -> Handler PaymentStatusResponse
buildPaymentStatus pool route = do
  hops <- liftIO $ Db.getRouteHops pool route.routeId
  pure
    PaymentStatusResponse
      { routeId = route.routeId
      , invoiceId = route.routeInvoiceId
      , senderAddress = route.routeSenderAddress
      , receiverAddress = route.routeReceiverAddress
      , amountLovelace = route.routeAmountLovelace
      , status = route.routeStatus
      , totalFee = route.routeTotalFee
      , network = route.routeNetwork
      , hops =
          [ HopStatusResponse
              { hopIndex = fromIntegral h.hopIndex
              , headId = h.hopHeadId
              , bridgeAddress = h.hopBridgeAddress
              , senderAddress = h.hopSenderAddress
              , receiverAddress = h.hopReceiverAddress
              , htlcStatus = h.hopHtlcStatus
              , htlcTxHash = h.hopHtlcTxHash
              , secretHash = h.hopSecretHash
              , preimage = h.hopPreimage
              , timeoutSlot = h.hopTimeoutSlot
              , fee = h.hopFeeLovelace
              , lockedAt = h.hopLockedAt
              , claimedAt = h.hopClaimedAt
              }
          | h <- hops
          ]
      , createdAt = route.routeCreatedAt
      , updatedAt = route.routeUpdatedAt
      }

-- | POST /api/v1/relay/preimage/:paymentHash
-- Submit a revealed preimage so bridge operators can claim their hops.
handleSubmitPreimage :: Pool -> Text -> SubmitPreimageRequest -> Handler MessageResponse
handleSubmitPreimage pool paymentHash req = do
  -- Validate: blake2b-256(preimage) must equal the payment hash
  -- For now, trust the caller — validation requires a blake2b binding.
  -- The on-chain script is the ultimate validator anyway.
  liftIO $ Db.setPreimageByHash pool paymentHash req.preimage
  pure $ MessageResponse "Preimage stored — bridge operators can now claim their hops"

-- ─── HTLC tx blueprint handlers ───
--
-- Slots reserved on either side of @timeoutSlot@ so that lock/claim
-- validity ranges leave room for ledger latency and clock skew. The
-- on-chain validator only checks @valid_before timeout@ for claims and
-- @valid_after timeout@ for refunds, so as long as we keep the bound
-- strict and not too tight, the hop is safe to execute.
htlcSafetyMarginSlots :: Int64
htlcSafetyMarginSlots = 60

-- | How far ahead of the chain tip we're willing to set a script-tx
-- validity bound. The head ledger refuses to translate slots that lie
-- past its known era horizon (typically @chainTip + safezone@,
-- ≈ 16h on Preview/Preprod/Mainnet); 4 hours leaves comfortable
-- headroom while still letting routes survive a reasonable submit
-- delay.
eraSafeWindowSlots :: Int64
eraSafeWindowSlots = 14400

-- | Minimum ADA we put on the HTLC lock output. The output carries the
-- inline HTLC datum and (for now) the inline reference script, so its
-- ledger-mandated minimum is roughly 5.6 ADA on preview/preprod params.
-- 7 ADA gives breathing room. When we move the validator to a shared
-- per-head ref-script UTxO, this floor drops back to ≈ 2 ADA.
htlcLockMinAdaLovelace :: Int64
htlcLockMinAdaLovelace = 7_000_000

-- | Clamp a script-tx validity-upper to both the HTLC's timeout
-- (with safety margin) and the head ledger's era horizon (chain tip
-- plus a safe window). If neither chain tip is known nor the timeout
-- is reachable, we return @0@ so the tx is rejected as invalid rather
-- than silently signing a ticking time bomb.
clampValidityUpper :: Int64 -> Int64 -> Int64
clampValidityUpper chainSlot timeoutSlot =
  let timeoutBound = timeoutSlot - htlcSafetyMarginSlots
      horizonBound = if chainSlot > 0 then chainSlot + eraSafeWindowSlots else timeoutBound
   in max 0 (min timeoutBound horizonBound)

-- | GET /api/v1/htlc/validator
handleHtlcValidator :: Maybe Text -> Handler HtlcValidatorResponse
handleHtlcValidator mCbor = do
  cbor <- case mCbor of
    Nothing ->
      throwError $ err500{errBody = Aeson.encode $ ErrorResponse "HTLC validator CBOR is not configured"}
    Just c -> pure c
  pure
    HtlcValidatorResponse
      { scriptHash = Htlc.htlcScriptHashHex
      , scriptCborHex = cbor
      , scriptType = "PlutusV3"
      }

-- | Look up route + sorted hops + sanity-check the hop index.
-- Returns the route, the hop, and the sorted list of hops (in case the
-- caller needs to compute downstream fees).
loadHop :: Pool -> Text -> Int -> Handler (PaymentRoute Identity, RouteHop Identity, [RouteHop Identity])
loadHop pool rid idx = do
  mRoute <- liftIO $ Db.getPaymentRoute pool rid
  route <- case mRoute of
    Nothing -> throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Route not found"}
    Just r -> pure r
  rawHops <- liftIO $ Db.getRouteHops pool rid
  let sorted = List.sortOn (\h -> h.hopIndex) rawHops
  case List.find (\h -> fromIntegral h.hopIndex == idx) sorted of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Hop not found in route"}
    Just hop -> pure (route, hop, sorted)

-- | POST /api/v1/relay/payments/:routeId/hops/:hopIndex/lock-tx
handleLockTx :: Pool -> TVar Int64 -> Text -> Int -> Handler LockTxBlueprint
handleLockTx pool chainSlotVar rid idx = do
  (route, hop, sorted) <- loadHop pool rid idx
  chainSlot <- liftIO $ readTVarIO chainSlotVar
  scriptAddr <- case Htlc.htlcScriptAddress route.routeNetwork of
    Left e ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse e}
    Right a -> pure a
  hashBytes <- decodeHopHash hop.hopSecretHash
  senderPkh <- decodeAddrPkh "sender" hop.hopSenderAddress
  receiverPkh <- decodeAddrPkh "receiver" hop.hopReceiverAddress
  -- The HTLC validator's @referenceScript@ bytes ride along in the lock
  -- output. We don't store them server-side per-route — they're the same
  -- validator for every hop on every network — so callers can either
  -- inline the CBOR returned here or fetch it once from /htlc/validator.
  -- We don't attach it to the lock blueprint to keep responses lean.
  let timeoutSlot = hop.hopTimeoutSlot
  -- The HTLC validator compares @datum.timeout@ against the tx's
  -- @validity_range@, which Plutus exposes as a 'POSIXTime' in
  -- milliseconds. So @datum.timeout@ has to be POSIX-ms too — not the
  -- slot we use elsewhere.
  timeoutMs <- case Slot.slotToPosixMs route.routeNetwork timeoutSlot of
    Just ms -> pure ms
    Nothing ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ "Unsupported network for slot conversion: " <> route.routeNetwork}
  let datumBytes = Htlc.mkDatumCbor hashBytes timeoutMs senderPkh receiverPkh
      -- Amount the locker funds the hop with: invoice amount plus the
      -- fees of every downstream hop (the bridges shave their cut as
      -- payment cascades forward). Floored at @htlcLockMinAdaLovelace@
      -- because the inline reference script pushes the output's
      -- ledger-mandated min-ada past small invoice amounts.
      downstreamFees =
        sum
          [ h.hopFeeLovelace
          | h <- sorted
          , fromIntegral h.hopIndex > idx
          ]
      lockAmount = max htlcLockMinAdaLovelace (route.routeAmountLovelace + downstreamFees)
  pure
    LockTxBlueprint
      { headId = hop.hopHeadId
      , scriptAddress = scriptAddr
      , scriptHash = Htlc.htlcScriptHashHex
      , datum =
          HtlcDatumView
            { paymentHash = hop.hopSecretHash
            , timeoutSlot = timeoutSlot
            , senderPkh = Htlc.hexEncode senderPkh
            , receiverPkh = Htlc.hexEncode receiverPkh
            }
      , datumCborHex = Htlc.hexEncode datumBytes
      , validatorRefScriptCborHex = "" -- caller fetches once via /htlc/validator
      , lockAmountLovelace = lockAmount
      , validityUpperSlot = clampValidityUpper chainSlot timeoutSlot
      , requiredSignerPkh = Htlc.hexEncode senderPkh
      }

-- | POST /api/v1/relay/payments/:routeId/hops/:hopIndex/claim-tx
handleClaimTx :: Pool -> TVar Int64 -> Text -> Int -> ClaimTxRequest -> Handler ClaimTxBlueprint
handleClaimTx pool chainSlotVar rid idx req = do
  (_route, hop, _) <- loadHop pool rid idx
  chainSlot <- liftIO $ readTVarIO chainSlotVar
  htlcTxHash <- requireHtlcTx hop
  receiverPkh <- decodeAddrPkh "receiver" hop.hopReceiverAddress
  preimageBytes <- case Base16.decode (T.encodeUtf8 req.preimage) of
    Left e ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ "preimage hex decode failed: " <> T.pack e}
    Right b -> pure b
  let redeemer = Htlc.mkClaimRedeemerCbor preimageBytes
      timeout = hop.hopTimeoutSlot
  pure
    ClaimTxBlueprint
      { headId = hop.hopHeadId
      , htlcInputTxHash = htlcTxHash
      , htlcInputIndex = 0
      , redeemerCborHex = Htlc.hexEncode redeemer
      , validityUpperSlot = clampValidityUpper chainSlot timeout
      , requiredSignerPkh = Htlc.hexEncode receiverPkh
      }

-- | POST /api/v1/relay/payments/:routeId/hops/:hopIndex/refund-tx
handleRefundTx :: Pool -> TVar Int64 -> Text -> Int -> Handler RefundTxBlueprint
handleRefundTx pool chainSlotVar rid idx = do
  (_route, hop, _) <- loadHop pool rid idx
  _ <- liftIO $ readTVarIO chainSlotVar  -- reserved for future clamp; refund-side
                                          -- has different semantics than claim
  htlcTxHash <- requireHtlcTx hop
  senderPkh <- decodeAddrPkh "sender" hop.hopSenderAddress
  let timeout = hop.hopTimeoutSlot
  pure
    RefundTxBlueprint
      { headId = hop.hopHeadId
      , htlcInputTxHash = htlcTxHash
      , htlcInputIndex = 0
      , redeemerCborHex = Htlc.hexEncode Htlc.refundRedeemerCbor
      , validityLowerSlot = timeout + htlcSafetyMarginSlots
      , requiredSignerPkh = Htlc.hexEncode senderPkh
      }

decodeAddrPkh :: Text -> Text -> Handler ByteString
decodeAddrPkh role addr = case Htlc.addressOrPkhToBytes addr of
  Left e ->
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ role <> " address: " <> e}
  Right b -> pure b

decodeHopHash :: Text -> Handler ByteString
decodeHopHash s = case Base16.decode (T.encodeUtf8 s) of
  Left e ->
    throwError $ err500{errBody = Aeson.encode $ ErrorResponse $ "secret hash hex decode failed: " <> T.pack e}
  Right b
    | BS.length b == 32 -> pure b
    | otherwise ->
        throwError $ err500{errBody = Aeson.encode $ ErrorResponse $ "secret hash must be 32 bytes, got " <> T.pack (show (BS.length b))}

requireHtlcTx :: RouteHop Identity -> Handler Text
requireHtlcTx hop = case hop.hopHtlcTxHash of
  Nothing ->
    throwError $ err409{errBody = Aeson.encode $ ErrorResponse "Hop has not been locked yet (htlc_tx_hash is null)"}
  Just t -> pure t
