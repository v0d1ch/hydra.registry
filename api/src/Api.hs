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
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Db qualified
import Db.Schema (ExplorerHead (..), Head (..), HeadParticipant (..), Invoice (..), PaymentRoute (..), RouteHop (..), Utxo (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraEvent (..))
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
import Servant

-- | Our own endpoints that live under /api/v1/
type ApiV1Routes =
  "health" :> Get '[JSON] HealthResponse
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
    -- Participant lookup
    :<|> "addresses" :> Capture "address" Text :> "heads" :> Get '[JSON] [ParticipantHeadInfo]
    -- Relay endpoints
    :<|> "relay" :> "invoices" :> ReqBody '[JSON] CreateInvoiceRequest :> Post '[JSON] InvoiceResponse
    :<|> "relay" :> "invoices" :> Capture "invoiceId" Text :> Get '[JSON] InvoiceResponse
    :<|> "relay" :> "routes" :> ReqBody '[JSON] FindRoutesRequest :> Post '[JSON] [RouteResponse]
    :<|> "relay" :> "routes" :> Capture "routeId" Text :> "execute" :> Post '[JSON] PaymentStatusResponse
    :<|> "relay" :> "payments" :> Capture "paymentId" Text :> Get '[JSON] PaymentStatusResponse

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
  , htlcScriptHash :: Maybe Text
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
    :<|> handleRegister env.logger env.pool env.eventQueue
    :<|> handleListHeads env.pool
    :<|> handleHeadDetail env.pool
    :<|> handleHeadAddresses env.pool
    :<|> handleAddressBalance env.pool
    :<|> handleHeadUtxos env.pool
    :<|> handleAdminDeleteHead env.pool
    :<|> handleMetrics env.metrics
    :<|> handleStats env.pool
    :<|> handleListExplorerHeads env.pool
    :<|> handleExplorerHeadDetail env.pool
    :<|> handleAddressHeads env.pool
    :<|> handleCreateInvoice env.pool
    :<|> handleGetInvoice env.pool
    :<|> handleFindRoutes env.pool env.relayGraph
    :<|> handleExecuteRoute env.pool
    :<|> handleGetPayment env.pool

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

-- | POST /api/v1/heads/register
handleRegister :: Logger -> Pool -> TQueue HydraEvent -> RegisterHead -> Handler RegisterHeadResponse
handleRegister logger pool eventQueue req = do
  let isBridge = maybe False id req.bridge
      bridgeFee = fromIntegral @Int @Int64 <$> req.feeLovelace
  result <- liftIO $ Indexer.registerHead logger pool eventQueue req.host req.port isBridge bridgeFee
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
  pure
    StatsResponse
      { headCount = hCount
      , totalUtxos = uCount
      , headsByStatus = byStatus
      , explorerHeadCount = explorerCount
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
handleListExplorerHeads :: Pool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler [ExplorerHeadInfo]
handleListExplorerHeads pool mCount mPage mStatus mNetwork = do
  let count = min 100 $ maybe 100 (max 1) mCount
      page = maybe 1 (max 1) mPage
  explorerHeads <- liftIO $ Db.getExplorerHeadsPaginated pool count page mStatus mNetwork
  registeredHeads <- liftIO $ Db.getAllHeads pool
  let registeredIds = Map.fromList [(h.headId, ()) | h <- registeredHeads]
  pure $ map (explorerHeadToInfo registeredIds) explorerHeads

-- | GET /api/v1/explorer/heads/:headId
handleExplorerHeadDetail :: Pool -> Text -> Handler ExplorerHeadInfo
handleExplorerHeadDetail pool hid = do
  mExplorer <- liftIO $ Db.getExplorerHead pool hid
  case mExplorer of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Explorer head not found"}
    Just eh -> do
      mRegistered <- liftIO $ Db.getHead pool hid
      pure $ explorerHeadToInfo (maybe Map.empty (\h -> Map.singleton h.headId ()) mRegistered) eh

-- | Convert an ExplorerHead DB row to API response
explorerHeadToInfo :: Map.Map Text () -> ExplorerHead Identity -> ExplorerHeadInfo
explorerHeadToInfo registeredIds eh =
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

-- | POST /api/v1/relay/invoices
handleCreateInvoice :: Pool -> CreateInvoiceRequest -> Handler InvoiceResponse
handleCreateInvoice pool req = do
  case validateAddress req.receiverAddress of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right _ -> pure ()
  when (req.amountLovelace <= 0) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "amount must be positive"}
  when (T.null req.paymentHash) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "paymentHash is required"}
  now <- liftIO getCurrentTime
  iid <- liftIO $ UUID.toText <$> UUID.nextRandom
  let expirySeconds = maybe 3600 id req.expiresInSeconds
      expiresAt = addUTCTime (fromIntegral expirySeconds) now
  liftIO $ Db.insertInvoice pool iid req.receiverAddress req.paymentHash req.amountLovelace req.memo "pending" expiresAt
  pure
    InvoiceResponse
      { invoiceId = iid
      , receiverAddress = req.receiverAddress
      , paymentHash = req.paymentHash
      , amountLovelace = req.amountLovelace
      , memo = req.memo
      , status = "pending"
      , expiresAt = expiresAt
      , createdAt = now
      }

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
          , receiverAddress = inv.invoiceReceiverAddress
          , paymentHash = inv.invoicePaymentHash
          , amountLovelace = inv.invoiceAmountLovelace
          , memo = inv.invoiceMemo
          , status = inv.invoiceStatus
          , expiresAt = inv.invoiceExpiresAt
          , createdAt = inv.invoiceCreatedAt
          }

-- | POST /api/v1/relay/routes
handleFindRoutes :: Pool -> TVar Graph.RelayGraph -> FindRoutesRequest -> Handler [RouteResponse]
handleFindRoutes pool graphVar req = do
  -- Look up the invoice
  mInvoice <- liftIO $ Db.getInvoice pool req.invoiceId
  case mInvoice of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Invoice not found"}
    Just inv -> do
      graph <- liftIO $ readTVarIO graphVar
      let routes = Graph.findRoutes graph req.senderAddress inv.invoiceReceiverAddress req.network 3
      mapM (routeToResponse pool req.invoiceId req.senderAddress inv.invoiceReceiverAddress inv.invoiceAmountLovelace inv.invoicePaymentHash req.network) routes

routeToResponse :: Pool -> Text -> Text -> Text -> Int64 -> Text -> Text -> Graph.Route -> Handler RouteResponse
routeToResponse pool invoiceId senderAddr receiverAddr amount paymentHash network route = do
  rid <- liftIO $ UUID.toText <$> UUID.nextRandom
  let hops =
        [ RouteHopResponse
            { headId = h.hopHeadId
            , bridgeAddress = h.hopBridgeAddress
            , fee = h.hopFee
            }
        | h <- route.routeHops
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
      (Aeson.toJSON hops)
      route.routeTotalFee
      network
  -- Persist hops
  hopRows <- liftIO $
    mapM
      ( \(idx, h) -> do
          hid <- UUID.toText <$> UUID.nextRandom
          pure (hid, rid, idx, h.hopHeadId, h.hopBridgeAddress, "pending", paymentHash, (0 :: Int64), h.hopFee)
      )
      (zip [0 ..] route.routeHops)
  liftIO $ Db.insertRouteHops pool hopRows
  pure
    RouteResponse
      { routeId = rid
      , hops = hops
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
              , htlcStatus = h.hopHtlcStatus
              , htlcTxHash = h.hopHtlcTxHash
              , secretHash = h.hopSecretHash
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
