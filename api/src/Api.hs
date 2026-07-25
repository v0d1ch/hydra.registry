module Api where

import Api.Types
import Api.Validation (validateAddress)
import Cache (Cache, insertCache, lookupCache)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.ByteString.Lazy qualified as BSL
import System.Process (readProcess)
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
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Text.Read (readMaybe)
import Data.UUID qualified as UUID
import Agent.CommandQueue (CommandWaiters, awaitCommand, resolveCommand)
import Data.UUID.V4 qualified as UUID
import Db qualified
import Db.Schema (AgentCommand (..), AgentRegistration (..), ExplorerHead (..), Head (..), HeadParticipant (..), Invoice (..), PaymentRoute (..), RouteHop (..), Utxo (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraEvent (..), normalizeHost, validateHydraNode)
import Hydra.Client qualified
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
import Hydra.Submit qualified as Submit
import Relay.EventBus (EventBus, RouteEvent (..))
import Relay.EventBus qualified as Bus
import Relay.Graph qualified as Graph
import Relay.Slot qualified as Slot
import Servant
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BSL
import Network.Wai (Application)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Tx.Builder qualified as Tx
import Control.Exception (SomeException, try)
import Crypto.Hash (Digest, SHA256, hash)
import Tx.Builder (extractPkhFromAddress)

-- | Our own endpoints that live under /api/v1/
type ApiV1Routes =
  "health" :> Get '[JSON] HealthResponse
    :<|> "heads" :> "check" :> QueryParam "host" Text :> QueryParam "port" Int :> Get '[JSON] CheckHeadResponse
    :<|> "heads" :> "register" :> ReqBody '[JSON] RegisterHead :> Post '[JSON] RegisterHeadResponse
    :<|> "heads" :> Capture "headId" Text :> "ref-script" :> ReqBody '[JSON] SetRefScriptRequest :> Post '[JSON] MessageResponse
    :<|> "heads" :> Capture "headId" Text :> "publish-ref-script-tx-cbor" :> ReqBody '[JSON] BuildTxFromWalletRequest :> Post '[JSON] Tx.BuildResult
    :<|> "heads" :> Capture "headId" Text :> "submit" :> ReqBody '[JSON] SubmitTxRequest :> Post '[JSON] Submit.SubmitResult
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
    :<|> "relay" :> "participants" :> Capture "pkh" Text :> "routes" :> Get '[JSON] [ParticipantRouteSummary]
    :<|> "relay" :> "participants" :> Capture "pkh" Text :> "invoices" :> Get '[JSON] [InvoiceResponse]
    :<|> "relay" :> "invoices" :> QueryParam "status" Text :> Get '[JSON] [InvoiceResponse]
    :<|> "relay" :> "invoices" :> ReqBody '[JSON] CreateInvoiceRequest :> Post '[JSON] InvoiceResponse
    :<|> "relay" :> "invoices" :> Capture "invoiceId" Text :> Get '[JSON] InvoiceResponse
    :<|> "relay" :> "routes" :> ReqBody '[JSON] FindRoutesRequest :> Post '[JSON] [RouteResponse]
    :<|> "relay" :> "routes" :> Capture "routeId" Text :> "execute" :> Post '[JSON] PaymentStatusResponse
    :<|> "relay" :> "payments" :> Capture "paymentId" Text :> Get '[JSON] PaymentStatusResponse
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "events" :> Raw
    :<|> "relay" :> "preimage" :> Capture "paymentHash" Text :> ReqBody '[JSON] SubmitPreimageRequest :> Post '[JSON] MessageResponse
    -- HTLC tx blueprints
    :<|> "htlc" :> "validator" :> Get '[JSON] HtlcValidatorResponse
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "lock-tx" :> Post '[JSON] LockTxBlueprint
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "claim-tx" :> ReqBody '[JSON] ClaimTxRequest :> Post '[JSON] ClaimTxBlueprint
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "refund-tx" :> Post '[JSON] RefundTxBlueprint
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "lock-tx-cbor" :> ReqBody '[JSON] BuildTxFromWalletRequest :> Post '[JSON] Tx.BuildResult
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "claim-tx-cbor" :> ReqBody '[JSON] BuildClaimTxRequest :> Post '[JSON] Tx.BuildResult
    :<|> "relay" :> "payments" :> Capture "routeId" Text :> "hops" :> Capture "hopIndex" Int :> "refund-tx-cbor" :> ReqBody '[JSON] BuildTxFromWalletRequest :> Post '[JSON] Tx.BuildResult
    :<|> "users" :> Capture "walletAddress" Text :> "keyhash" :> Get '[JSON] UserKeyHashResponse
    :<|> "users" :> Capture "walletAddress" Text :> "keyhash" :> ReqBody '[JSON] SetKeyHashRequest :> Put '[JSON] UserKeyHashResponse
    -- Agent push model
    :<|> "agent" :> "register" :> ReqBody '[JSON] AgentRegisterRequest :> Post '[JSON] AgentRegisterResponse
    :<|> "agent" :> "events" :> Header "Authorization" Text :> Header "X-Agent-Binary-Hash" Text :> ReqBody '[JSON] AgentEventRequest :> Post '[JSON] MessageResponse
    :<|> "agent" :> "heads" :> Capture "headId" Text :> "protocol-parameters" :> Header "Authorization" Text :> Header "X-Agent-Binary-Hash" Text :> ReqBody '[JSON] Aeson.Value :> Put '[JSON] MessageResponse
    :<|> "agent" :> "commands" :> "poll" :> Header "Authorization" Text :> Header "X-Agent-Binary-Hash" Text :> Post '[JSON] [AgentCommandInfo]
    :<|> "agent" :> "commands" :> Capture "commandId" Text :> "result" :> Header "Authorization" Text :> Header "X-Agent-Binary-Hash" Text :> ReqBody '[JSON] Submit.SubmitResult :> Post '[JSON] MessageResponse
    -- Head ownership via L1 deposit
    :<|> "heads" :> Capture "headId" Text :> "claim-ownership" :> ReqBody '[JSON] ClaimOwnershipRequest :> Post '[JSON] ClaimOwnershipResponse

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
  , commandWaiters :: CommandWaiters
  , addressCache :: Cache [UtxoResponse]
  , staticDir :: FilePath
  , relayGraph :: TVar Graph.RelayGraph
  , -- | Highest L1 chain slot seen across any registered head's
    -- Greetings. Bumped by the Indexer; read by handlers that need to
    -- derive timeouts/validity bounds from chain time rather than the
    -- registry's local system clock.
    latestChainSlot :: TVar Int64
  , -- | Fan-out broadcast for relay state transitions. The HTLC
    -- watcher publishes lock/claim/completion events; the preimage
    -- submission handler publishes preimage reveals; SSE subscribers
    -- read filtered streams via 'subscribe'.
    relayEventBus :: EventBus
  , htlcScriptHash :: Maybe Text
  , htlcScriptCbor :: Maybe Text
  , cardanoNodeSocket :: Maybe FilePath
  , cardanoNodeMagic :: Maybe Int
  , agentAllowedHashes :: [Text]
  , directWs :: Bool
  -- ^ Allow dialing user hydra-node APIs directly (dev/testnet only).
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
  handleHealth env.pool env.latestChainSlot env.cardanoNodeSocket env.cardanoNodeMagic
    :<|> handleCheckHead env.directWs env.logger env.pool
    :<|> handleRegister env.directWs env.logger env.pool env.eventQueue
    :<|> handleSetRefScript env.pool
    :<|> handlePublishRefTxCbor env.directWs env.pool env.htlcScriptCbor
    :<|> handleSubmitTx env.directWs env.pool env.commandWaiters
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
    :<|> handleParticipantRoutes env.pool env.latestChainSlot
    :<|> handleParticipantInvoices env.pool
    :<|> handleGetInvoices env.pool
    :<|> handleCreateInvoice env.pool env.relayGraph
    :<|> handleGetInvoice env.pool
    :<|> handleFindRoutes env.pool env.relayGraph env.latestChainSlot
    :<|> handleExecuteRoute env.pool
    :<|> handleGetPayment env.pool
    :<|> handlePaymentEventStream env.relayEventBus
    :<|> handleSubmitPreimage env.pool env.relayEventBus
    :<|> handleHtlcValidator env.htlcScriptCbor
    :<|> handleLockTx env.pool env.latestChainSlot
    :<|> handleClaimTx env.pool env.latestChainSlot
    :<|> handleRefundTx env.pool env.latestChainSlot
    :<|> handleLockTxCbor env.directWs env.pool env.latestChainSlot env.htlcScriptCbor
    :<|> handleClaimTxCbor env.directWs env.pool env.latestChainSlot
    :<|> handleRefundTxCbor env.directWs env.pool env.latestChainSlot
    :<|> handleGetUserKeyHash env.pool
    :<|> handleSetUserKeyHash env.pool
    :<|> handleAgentRegister env.pool env.agentAllowedHashes
    :<|> handleAgentEvent env.pool env.agentAllowedHashes env.eventQueue
    :<|> handleAgentPushPParams env.pool env.agentAllowedHashes
    :<|> handleAgentPollCommands env.pool env.agentAllowedHashes
    :<|> handleAgentCommandResult env.pool env.agentAllowedHashes env.commandWaiters
    :<|> handleClaimOwnership env.pool

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
handleHealth :: Pool -> TVar Int64 -> Maybe FilePath -> Maybe Int -> Handler HealthResponse
handleHealth pool chainSlotVar mSocket mMagic = do
  heads <- liftIO $ Db.getAllHeads pool
  dbOk <- liftIO $ Db.checkDbConnectivity pool
  chainSlot <- liftIO $ readTVarIO chainSlotVar
  syncProgress <- liftIO $ queryNodeSyncProgress mSocket mMagic
  pure $
    HealthResponse
      { status = if dbOk then "ok" else "degraded"
      , headCount = length heads
      , dbConnected = dbOk
      , chainSlotKnown = chainSlot > 0
      , nodeSyncProgress = syncProgress
      }

queryNodeSyncProgress :: Maybe FilePath -> Maybe Int -> IO (Maybe Double)
queryNodeSyncProgress Nothing _ = pure Nothing
queryNodeSyncProgress _ Nothing = pure Nothing
queryNodeSyncProgress (Just socketPath) (Just magic) = do
  result <- try @SomeException $ do
    let args = ["query", "tip", "--testnet-magic", show magic, "--socket-path", socketPath]
    out <- readProcess "cardano-cli" args ""
    case Aeson.decode (BSL.fromStrict $ T.encodeUtf8 $ T.pack out) of
      Just obj -> pure $ KM.lookup "syncProgress" obj >>= \case
        Aeson.String s -> readMaybe (T.unpack s)
        _              -> Nothing
      Nothing  -> pure Nothing
  pure $ case result of
    Left _  -> Nothing
    Right v -> v

-- | GET /api/v1/heads/check?host=...&port=...
handleCheckHead :: Bool -> Logger -> Pool -> Maybe Text -> Maybe Int -> Handler CheckHeadResponse
handleCheckHead allowDirect logger pool mHost mPort = do
  requireDirectWs allowDirect
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
handleRegister :: Bool -> Logger -> Pool -> TQueue HydraEvent -> RegisterHead -> Handler RegisterHeadResponse
handleRegister allowDirect logger pool eventQueue req = do
  requireDirectWs allowDirect
  result <- liftIO $ Indexer.registerHead logger pool eventQueue req.host req.port req.walletAddress
  case result of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right HeadGreetings{greeterHeadId} ->
      pure $ RegisterHeadResponse greeterHeadId "connected"
    Right _ ->
      throwError $ err500{errBody = Aeson.encode $ ErrorResponse "Unexpected response"}

-- | POST /api/v1/heads/:headId/ref-script
-- Operator (or bridge agent) reports the @"txhash#ix"@ of the L2 UTxO
-- inside this head that holds the HTLC validator as an inline reference
-- script. Future lock blueprints for hops in this head omit the inline
-- script and point claims at this UTxO via @--spending-tx-in-reference@.
handleSetRefScript :: Pool -> Text -> SetRefScriptRequest -> Handler MessageResponse
handleSetRefScript pool hid req = do
  -- Validate "txhash#ix" shape: 64 hex chars, '#', non-negative int.
  case T.splitOn "#" req.utxo of
    [txh, ixT]
      | T.length txh == 64
      , T.all isHex txh
      , Just (_ :: Int) <- readMaybe (T.unpack ixT) -> pure ()
    _ -> throwError $ err400{errBody = Aeson.encode $ ErrorResponse "utxo must be in \"<txhash hex>#<index>\" form"}
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ -> do
      liftIO $ Db.setHeadRefScriptUtxo pool hid (Just req.utxo)
      pure $ MessageResponse $ "Ref-script UTxO recorded for head " <> hid
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

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
          , htlcEnabled = isJust h.headRefScriptUtxo
          , refScriptUtxo = h.headRefScriptUtxo
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
-- Returns all Open heads on the requested network (explorer-observed or
-- locally registered), with edges between heads sharing a participant.
-- Heads without participants appear as unconnected nodes — participant
-- data may be missing entirely (e.g. the chain observer can only fully
-- parse Init txs of its own hydra version). Deduplicates edges and caps
-- output to keep responses fast.
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
      -- Build head→participants and head→committed lovelace maps
      headParticipants :: Map.Map Text [Text]
      headParticipants =
        Map.fromListWith (<>)
          [ (p.participantHeadId, [p.participantAddress])
          | p <- participants
          , Set.member p.participantHeadId headIds
          ]
      headLovelace :: Map.Map Text Int64
      headLovelace =
        Map.fromListWith (+)
          [ (p.participantHeadId, p.participantCommittedLovelace)
          | p <- participants
          , Set.member p.participantHeadId headIds
          ]
  let explorerNodeIds = Set.fromList [eh.explorerHeadId | eh <- networkHeads]
      explorerNodes =
        [ SubgraphNode
            { headId = eh.explorerHeadId
            , network = eh.explorerNetwork
            , hasHtlc = Set.member eh.explorerHeadId htlcIds
            , isUserHead = Set.member eh.explorerHeadId registeredOpenIds
            , participants = Map.findWithDefault [] eh.explorerHeadId headParticipants
            , committedLovelace = Map.findWithDefault 0 eh.explorerHeadId headLovelace
            }
        | eh <- networkHeads
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
        , not (Set.member h.headId explorerNodeIds)
        ]
  pure
    SubgraphResponse
      { nodes = explorerNodes <> registeredOnlyNodes
      , edges = dedupedEdges
      }

-- ─── Participant dashboard ───
--
-- Action-eligibility rules in one place so the SPA never has to
-- second-guess them. The dashboard shows a card per route the pkh
-- touches plus a (possibly empty) list of actions.

-- | The chain-time-relative urgency of a hop deadline, expressed as
-- a coarse bucket so the UI doesn't need to do slot math. Tiers
-- mirror what feels natural for a 1-second-slot network: "expiring"
-- inside 5 minutes, "soon" inside 30 minutes, "ok" otherwise.
hopUrgency :: Int64 -> Int64 -> Text
hopUrgency chainSlot timeoutSlot
  | chainSlot <= 0 = "ok" -- chain tip not yet known; no point alarming
  | chainSlot >= timeoutSlot = "expired"
  | timeoutSlot - chainSlot <= 300 = "expiring"
  | timeoutSlot - chainSlot <= 1800 = "soon"
  | otherwise = "ok"

-- | One role per route. We emit a list because in principle a pkh
-- could play two roles in the same route (e.g. self-pay) — the
-- dashboard renders the list.
routeRolesFor :: Text -> PaymentRoute Identity -> [RouteHop Identity] -> [Text]
routeRolesFor me route hops =
  [ r
  | (cond, r) <-
      [ (route.routeSenderAddress == me, "sender")
      , (route.routeReceiverAddress == me, "receiver")
      ,
        ( me /= route.routeSenderAddress
            && me /= route.routeReceiverAddress
            && any (\h -> h.hopSenderAddress == me || h.hopReceiverAddress == me) hops
        , "bridge"
        )
      ]
  , cond
  ]

-- exported for tests; the type stays the same as the call-site form
-- so the spec can build minimal RouteHop fixtures.

-- | Compute every action the participant can take on this route.
-- Logic per hop H, given my pkh @me@:
--
-- * I am the locker (@H.sender == me@):
--     * @H.status == "pending"@ AND upstream is locked-or-claimed → "lock"
--     * @H.status == "locked"@  AND timeout passed AND no preimage → "refund"
-- * I am the claimer (@H.receiver == me@):
--     * @H.status == "locked"@  AND preimage is in DB → "claim"
--
-- The "upstream condition" for non-zero hops keeps a bridge from
-- locking before the previous bridge (or sender) has locked. Hop 0
-- has no upstream.
participantActionsFor
  :: Int64
  -> Text
  -> [RouteHop Identity]
  -> [ParticipantAction]
participantActionsFor chainSlot me hops =
  let sortedHops = List.sortOn (.hopIndex) hops
      indexed = zip [0 :: Int ..] sortedHops
      hopAt idx = lookup idx indexed
      upstreamReady i
        | i == 0 = True
        | otherwise = case hopAt (i - 1) of
            Just h -> h.hopHtlcStatus == "locked" || h.hopHtlcStatus == "claimed"
            Nothing -> False
      mkAction h kind' =
        ParticipantAction
          { hopIndex = fromIntegral h.hopIndex
          , kind = kind'
          , urgency = hopUrgency chainSlot h.hopTimeoutSlot
          }
   in concat
        [ -- Lock side (I am the sender of the hop).
          [ mkAction h "lock"
          | (i, h) <- indexed
          , h.hopSenderAddress == me
          , h.hopHtlcStatus == "pending"
          , upstreamReady i
          ]
        , -- Refund side (I locked, timeout passed, never claimed).
          [ mkAction h "refund"
          | (_i, h) <- indexed
          , h.hopSenderAddress == me
          , h.hopHtlcStatus == "locked"
          , chainSlot > 0
          , chainSlot >= h.hopTimeoutSlot
          , Nothing == h.hopPreimage
          ]
        , -- Claim side (I am the receiver of a still-locked hop and
          -- the preimage is in the DB so the script will actually
          -- pass).
          [ mkAction h "claim"
          | (_i, h) <- indexed
          , h.hopReceiverAddress == me
          , h.hopHtlcStatus == "locked"
          , Just _ <- [h.hopPreimage]
          ]
        ]

-- | GET /api/v1/relay/participants/:pkh/routes
--
-- Dashboard feed: every route this pkh touches (as sender, bridge,
-- or receiver), each annotated with the participant's role(s) and
-- a list of actions they can take right now.
handleParticipantRoutes :: Pool -> TVar Int64 -> Text -> Handler [ParticipantRouteSummary]
handleParticipantRoutes pool chainSlotVar pkh = do
  pairs <- liftIO $ Db.getRoutesByParticipantPkh pool pkh
  chainSlot <- liftIO $ readTVarIO chainSlotVar
  pure $ map (toSummary chainSlot) pairs
 where
  toSummary chainSlot (r, hops) =
    ParticipantRouteSummary
      { route =
          PaymentStatusResponse
            { routeId = r.routeId
            , invoiceId = r.routeInvoiceId
            , senderAddress = r.routeSenderAddress
            , receiverAddress = r.routeReceiverAddress
            , amountLovelace = r.routeAmountLovelace
            , status = r.routeStatus
            , totalFee = r.routeTotalFee
            , network = r.routeNetwork
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
                | h <- List.sortOn (.hopIndex) hops
                ]
            , createdAt = r.routeCreatedAt
            , updatedAt = r.routeUpdatedAt
            }
      , roles = routeRolesFor pkh r hops
      , actions = participantActionsFor chainSlot pkh hops
      }

invoiceToResponse :: Invoice Identity -> InvoiceResponse
invoiceToResponse inv =
  InvoiceResponse
    { invoiceId         = inv.invoiceId
    , headId            = inv.invoiceHeadId
    , receiverOnChainId = inv.invoiceReceiverOnChainId
    , paymentHash       = inv.invoicePaymentHash
    , amountLovelace    = inv.invoiceAmountLovelace
    , memo              = inv.invoiceMemo
    , status            = inv.invoiceStatus
    , expiresAt         = inv.invoiceExpiresAt
    , createdAt         = inv.invoiceCreatedAt
    }

-- | GET /api/v1/relay/participants/:pkh/invoices
handleParticipantInvoices :: Pool -> Text -> Handler [InvoiceResponse]
handleParticipantInvoices pool pkh = do
  invoices <- liftIO $ Db.getInvoicesByReceiver pool pkh
  pure $ map invoiceToResponse invoices

-- | GET /api/v1/relay/invoices?status=pending
handleGetInvoices :: Pool -> Maybe Text -> Handler [InvoiceResponse]
handleGetInvoices pool mStatus = do
  invs <- liftIO $ Db.getInvoicesByStatus pool (fromMaybe "pending" mStatus)
  pure $ map invoiceToResponse invs

-- | POST /api/v1/relay/invoices
handleCreateInvoice :: Pool -> TVar Graph.RelayGraph -> CreateInvoiceRequest -> Handler InvoiceResponse
handleCreateInvoice pool graphVar req = do
  when (T.null req.headId) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "headId is required"}
  when (T.length req.receiverOnChainId /= 56 || not (T.all isHex req.receiverOnChainId)) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "receiverOnChainId must be 56 hex chars (28-byte vkey hash)"}
  when (req.amountLovelace <= 0) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "amount must be positive"}
  when (T.null req.paymentHash) $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "paymentHash is required"}
  mHead <- liftIO $ Db.getHead pool req.headId
  case mHead of
    Nothing ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse "headId does not exist"}
    Just h -> do
      when (h.headStatus /= "Open") $
        throwError $ err400{errBody = Aeson.encode $ ErrorResponse "Head is not Open — wait for it to be fully connected before creating an invoice"}
      when (isNothing h.headRefScriptUtxo) $
        throwError $ err400{errBody = Aeson.encode $ ErrorResponse "Head does not have an HTLC ref script published — publish the validator first (Setup → step 04)"}
  graph <- liftIO $ readTVarIO graphVar
  let routable = any (\e -> e.edgeFromHead == req.headId || e.edgeToHead == req.headId) graph.graphEdges
  unless routable $
    throwError $ err400{errBody = Aeson.encode $ ErrorResponse "This head has no routing connections — at least one other head must share a bridge participant before invoices can be created"}
  now <- liftIO getCurrentTime
  iid <- liftIO $ UUID.toText <$> UUID.nextRandom
  let expirySeconds = maybe 3600 id req.expiresInSeconds
      expiresAt = addUTCTime (fromIntegral expirySeconds) now
  liftIO $ Db.insertInvoice pool iid req.headId req.receiverOnChainId req.paymentHash req.amountLovelace req.memo "pending" expiresAt
  pure
    InvoiceResponse
      { invoiceId = iid
      , headId = req.headId
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
      pure $ invoiceToResponse inv

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
      -- Participants are identified by their OnChainId (pkh of --cardano-signing-key).
      -- Sender's OnChainId comes from the request, receiver's from the invoice.
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
      numHops = length htlcs
  -- Receiver-side hop (largest @hopIndex@) needs a smaller timeout
  -- than upstream hops so the bridge has time to react after seeing
  -- the preimage downstream. Anchor the *downstream-most* hop at the
  -- invoice deadline and step each upstream hop later by
  -- 'hopTimeoutMarginSlots'.
  baseTimeoutSlot <-
    if chainSlot > 0
      then pure (chainSlot + secondsRemaining)
      else case Slot.utcTimeToSlot network expiresAt of
        Just slot -> pure slot
        Nothing ->
          throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ "Unsupported network for slot conversion: " <> network}
  let timeoutForHop i = hopTimeoutSlot baseTimeoutSlot numHops i
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
            , timeoutForHop (fromIntegral i)
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

-- | GET /api/v1/relay/payments/:routeId/events  (text/event-stream)
--
-- Long-lived Server-Sent Events stream that pushes 'RouteEvent's
-- relevant to a single payment route. Each line is one SSE record;
-- the @event:@ name is the constructor tag (e.g. @HopLocked@) and the
-- @data:@ payload is the JSON-encoded event.
--
-- The handler 'subscribe's to the bus and filters in-process — there
-- is no DB read on every event. The browser's @EventSource@ handles
-- reconnect; events 'publish'ed while a client was disconnected are
-- not replayed (the client reconciles via a fresh @GET /payments/{r}@).
handlePaymentEventStream :: EventBus -> Text -> Tagged Handler Application
handlePaymentEventStream bus rid = Tagged $ \req respond -> do
  busChan <- Bus.subscribe bus
  let nextEvent :: IO ServerEvent
      nextEvent = do
        ev <- atomically (readTChan busChan)
        if Bus.routeEventRouteId ev == rid
          then
            pure
              ServerEvent
                { eventName = Just (BB.byteString (T.encodeUtf8 (Bus.routeEventTag ev)))
                , eventId = Nothing
                , eventData = [BB.lazyByteString (Aeson.encode ev)]
                }
          else nextEvent
  eventSourceAppIO nextEvent req respond

-- | POST /api/v1/relay/preimage/:paymentHash
-- Submit a revealed preimage so bridge operators can claim their hops.
handleSubmitPreimage :: Pool -> EventBus -> Text -> SubmitPreimageRequest -> Handler MessageResponse
handleSubmitPreimage pool bus paymentHash req = do
  -- Validate: blake2b-256(preimage) must equal the payment hash
  -- For now, trust the caller — validation requires a blake2b binding.
  -- The on-chain script is the ultimate validator anyway.
  liftIO $ Db.setPreimageByHash pool paymentHash req.preimage
  -- Fire one PreimageRevealed event per distinct route this payment
  -- hash now unblocks. SSE subscribers on those routes will
  -- light up the receiver's claim button (and, downstream, every
  -- bridge's upstream claim button).
  affected <- liftIO $ Db.getRouteIdsByPaymentHash pool paymentHash
  liftIO $ mapM_ (\rid -> Bus.publish bus PreimageRevealed{routeId = rid, paymentHash}) affected
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

-- | Minimum ADA we put on the HTLC lock output when the validator is
-- inlined as the output's reference script. ≈ 5.6 ADA is the
-- ledger-mandated floor for ~1 KB of ref script + 96-byte datum on
-- Preview/Preprod params; 7 ADA gives a small cushion. Used only when
-- the head has no shared ref-script UTxO published.
htlcLockMinAdaInlineLovelace :: Int64
htlcLockMinAdaInlineLovelace = 7_000_000

-- | Minimum ADA when the lock output omits the inline ref script and
-- the validator is referenced from the head's shared ref-script UTxO.
-- Just script-addr + value + 96-byte datum → ≈ 1.5 ADA floor; 2 ADA
-- is enough headroom for any reasonable parameter shift.
htlcLockMinAdaSharedLovelace :: Int64
htlcLockMinAdaSharedLovelace = 2_000_000

-- | Slot margin between adjacent hops' timeouts. The receiver-side hop
-- (largest @hopIndex@) gets the smallest timeout; each upstream hop is
-- 'hopTimeoutMarginSlots' later, so a bridge has time to react after
-- seeing the preimage downstream before the upstream lock can refund.
-- 600 slots ≈ 10 minutes on networks with 1s/slot.
hopTimeoutMarginSlots :: Int64
hopTimeoutMarginSlots = 600

-- | Deadline slot for hop @i@ in a route of @numHops@ hops, anchored
-- so the *downstream-most* hop (@i = numHops - 1@) lands at
-- 'baseSlot'. Every upstream hop is 'hopTimeoutMarginSlots' later so
-- a bridge has time to claim upstream after seeing the preimage land
-- downstream.
--
-- Property: timeouts are strictly monotone-decreasing in @i@ as long
-- as 'hopTimeoutMarginSlots > 0', which guarantees the cascade has
-- the correct safety ordering for any fee/path topology.
hopTimeoutSlot :: Int64 -> Int -> Int -> Int64
hopTimeoutSlot baseSlot numHops i =
  baseSlot + fromIntegral (numHops - 1 - i) * hopTimeoutMarginSlots

-- | Recommended fee floor for a non-Plutus tx (lock side). Locks just
-- spend a wallet UTxO into a script-address output with inline datum
-- and (optionally) inline ref script, so 300_000 lovelace covers
-- typical ~ 200-byte tx-bodies on Conway protocol params.
recommendedLockFeeLovelace :: Int64
recommendedLockFeeLovelace = 300_000

-- | Recommended fee floor for a Plutus-spending tx (claim/refund). The
-- script execution dominates: with our small validator the ledger
-- demands ~ 1.15 ADA, 1.5 ADA gives margin against future param
-- changes.
recommendedScriptFeeLovelace :: Int64
recommendedScriptFeeLovelace = 1_500_000

-- | Required collateral pledge for a script-spending tx. Cardano
-- mandates @ceil(fee * collateralPercentage / 100)@ where
-- @collateralPercentage = 150@, giving 2.25 ADA for a 1.5 ADA fee. We
-- round up to 2.5 ADA so a single 5 ADA collateral input with a
-- return-collateral output is enough.
recommendedCollateralLovelace :: Int64
recommendedCollateralLovelace = 2_500_000

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
  -- If the head has a published shared ref-script UTxO, the lock output
  -- doesn't need to inline the validator — that drops the lock's
  -- min-ada from ≈ 5.6 ADA to ≈ 1.5 ADA, which makes small invoices
  -- viable. The bridge agent / operator publishes the UTxO once and
  -- registers it via @POST /heads/{id}/ref-script@.
  mHead <- liftIO $ Db.getHead pool hop.hopHeadId
  let mRefScript = mHead >>= (.headRefScriptUtxo)
      timeoutSlot = hop.hopTimeoutSlot
  -- The HTLC validator compares @datum.timeout@ against the tx's
  -- @validity_range@, which Plutus exposes as a 'POSIXTime' in
  -- milliseconds. So @datum.timeout@ has to be POSIX-ms too — not the
  -- slot we use elsewhere.
  timeoutMs <- case Slot.slotToPosixMs route.routeNetwork timeoutSlot of
    Just ms -> pure ms
    Nothing ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ "Unsupported network for slot conversion: " <> route.routeNetwork}
  let datumBytes = Htlc.mkDatumCbor hashBytes timeoutMs senderPkh receiverPkh
      downstreamFees =
        sum
          [ h.hopFeeLovelace
          | h <- sorted
          , fromIntegral h.hopIndex > idx
          ]
      minAdaFloor = case mRefScript of
        Just _ -> htlcLockMinAdaSharedLovelace
        Nothing -> htlcLockMinAdaInlineLovelace
      lockAmount = max minAdaFloor (route.routeAmountLovelace + downstreamFees)
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
      , refScriptUtxo = mRefScript
      , lockAmountLovelace = lockAmount
      , validityUpperSlot = clampValidityUpper chainSlot timeoutSlot
      , requiredSignerPkh = Htlc.hexEncode senderPkh
      , recommendedFeeLovelace = recommendedLockFeeLovelace
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
  mHead <- liftIO $ Db.getHead pool hop.hopHeadId
  let mRefScript = mHead >>= (.headRefScriptUtxo)
      redeemer = Htlc.mkClaimRedeemerCbor preimageBytes
      timeout = hop.hopTimeoutSlot
  pure
    ClaimTxBlueprint
      { headId = hop.hopHeadId
      , htlcInputTxHash = htlcTxHash
      , htlcInputIndex = 0
      , redeemerCborHex = Htlc.hexEncode redeemer
      , refScriptUtxo = mRefScript
      , validityUpperSlot = clampValidityUpper chainSlot timeout
      , requiredSignerPkh = Htlc.hexEncode receiverPkh
      , recommendedFeeLovelace = recommendedScriptFeeLovelace
      , collateralRequiredLovelace = recommendedCollateralLovelace
      }

-- | POST /api/v1/relay/payments/:routeId/hops/:hopIndex/refund-tx
handleRefundTx :: Pool -> TVar Int64 -> Text -> Int -> Handler RefundTxBlueprint
handleRefundTx pool chainSlotVar rid idx = do
  (_route, hop, _) <- loadHop pool rid idx
  _ <- liftIO $ readTVarIO chainSlotVar  -- reserved for future clamp; refund-side
                                          -- has different semantics than claim
  htlcTxHash <- requireHtlcTx hop
  senderPkh <- decodeAddrPkh "sender" hop.hopSenderAddress
  mHead <- liftIO $ Db.getHead pool hop.hopHeadId
  let mRefScript = mHead >>= (.headRefScriptUtxo)
      timeout = hop.hopTimeoutSlot
  pure
    RefundTxBlueprint
      { headId = hop.hopHeadId
      , htlcInputTxHash = htlcTxHash
      , htlcInputIndex = 0
      , redeemerCborHex = Htlc.hexEncode Htlc.refundRedeemerCbor
      , refScriptUtxo = mRefScript
      , validityLowerSlot = timeout + htlcSafetyMarginSlots
      , requiredSignerPkh = Htlc.hexEncode senderPkh
      , recommendedFeeLovelace = recommendedScriptFeeLovelace
      , collateralRequiredLovelace = recommendedCollateralLovelace
      }

-- ─── Server-built tx CBOR (cardano-cli shell-out) ────────────────────
--
-- These four handlers and the @/heads/{id}/submit@ handler below
-- are what powers the SPA's "download tx, sign offline, upload"
-- flow. They produce a Conway-envelope JSON the user saves to a
-- file and signs with their own keys; the registry never sees
-- those keys.

-- | Fetch the head's protocol parameters via its @/protocol-parameters@
-- HTTP endpoint and return the JSON body verbatim. Caller passes it
-- straight into 'Tx.Builder' which writes it to a temp file before
-- @cardano-cli build-raw@ reads it.
fetchProtocolParams :: Text -> Int -> IO (Either Text Text)
fetchProtocolParams host portNum = do
  res <- try @SomeException $ do
    manager <- newManager defaultManagerSettings
    let url = "http://" <> T.unpack (Hydra.Client.normalizeHost host) <> ":" <> show portNum <> "/protocol-parameters"
    req <- parseRequest url
    httpLbs req manager
  case res of
    Left e -> pure (Left ("protocol-parameters fetch failed: " <> T.pack (show e)))
    Right resp -> case statusCode (responseStatus resp) of
      200 -> pure $ Right $ T.decodeUtf8 (BSL.toStrict (responseBody resp))
      n -> pure (Left ("protocol-parameters: HTTP " <> T.pack (show n)))

-- | Pick the smallest pure-ADA UTxO at @walletAddr@ inside @headId@
-- that holds at least @needed@ lovelace. Returns 409 to the caller
-- when none is suitable so the SPA can show a clear "deposit more
-- funds first" message.
pickInputUtxo :: Pool -> Text -> Text -> Int64 -> Handler (Utxo Identity)
pickInputUtxo pool hid walletAddr needed = do
  utxos <- liftIO $ Db.findWalletPureAdaUtxos pool hid walletAddr
  case List.find (\u -> u.utxoLovelace >= needed) utxos of
    Just u -> pure u
    Nothing ->
      throwError $
        err409
          { errBody =
              Aeson.encode $
                ErrorResponse $
                  "no pure-ADA UTxO at "
                    <> walletAddr
                    <> " in head "
                    <> hid
                    <> " with at least "
                    <> T.pack (show needed)
                    <> " lovelace"
          }

-- | Pick a pure-ADA UTxO suitable for collateral — at least
-- @needed@ lovelace, and explicitly *different* from the input
-- UTxO so we don't trip BabbageNonDisjointRefInputs / etc.
pickCollateralUtxo :: Pool -> Text -> Text -> Int64 -> (Text, Int32) -> Handler (Utxo Identity)
pickCollateralUtxo pool hid walletAddr needed (excludeTx, excludeIx) = do
  utxos <- liftIO $ Db.findWalletPureAdaUtxos pool hid walletAddr
  case List.find suitable utxos of
    Just u -> pure u
    Nothing ->
      throwError $
        err409
          { errBody =
              Aeson.encode $
                ErrorResponse $
                  "no separate pure-ADA UTxO at "
                    <> walletAddr
                    <> " in head "
                    <> hid
                    <> " for collateral (need ≥ "
                    <> T.pack (show needed)
                    <> " lovelace, distinct from input "
                    <> excludeTx
                    <> "#"
                    <> T.pack (show excludeIx)
                    <> ")"
          }
  where
    suitable u =
      u.utxoLovelace >= needed
        && not (u.utxoTxHash == excludeTx && u.utxoOutputIndex == excludeIx)

-- | Plutus V3 envelope JSON for an arbitrary script CBOR hex —
-- shape that @cardano-cli@ expects in @--tx-out-reference-script-file@
-- and friends.
plutusEnvelopeJson :: Text -> Aeson.Value
plutusEnvelopeJson cborHex =
  Aeson.object
    [ Key.fromString "type" Aeson..= ("PlutusScriptV3" :: Text)
    , Key.fromString "description" Aeson..= ("HTLC validator" :: Text)
    , Key.fromString "cborHex" Aeson..= cborHex
    ]

-- | Look up a head row, error 404 if not present.
loadHeadRow :: Pool -> Text -> Handler (Head Identity)
loadHeadRow pool hid = do
  m <- liftIO $ Db.getHead pool hid
  case m of
    Nothing -> throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just h -> pure h

-- | POST /api/v1/relay/payments/:routeId/hops/:hopIndex/lock-tx-cbor
handleLockTxCbor
  :: Bool
  -> Pool
  -> TVar Int64
  -> Maybe Text -- htlcScriptCbor
  -> Text
  -> Int
  -> BuildTxFromWalletRequest
  -> Handler Tx.BuildResult
handleLockTxCbor allowDirect pool chainSlotVar mScriptCbor rid idx req = do
  (route, hop, sorted) <- loadHop pool rid idx
  chainSlot <- liftIO $ readTVarIO chainSlotVar
  scriptAddr <- case Htlc.htlcScriptAddress route.routeNetwork of
    Left e -> throwError $ err400{errBody = Aeson.encode $ ErrorResponse e}
    Right a -> pure a
  hashBytes <- decodeHopHash hop.hopSecretHash
  senderPkh <- decodeAddrPkh "sender" hop.hopSenderAddress
  receiverPkh <- decodeAddrPkh "receiver" hop.hopReceiverAddress
  headRow <- loadHeadRow pool hop.hopHeadId
  let mRefScript = headRow.headRefScriptUtxo
      timeoutSlot = hop.hopTimeoutSlot
  timeoutMs <- case Slot.slotToPosixMs route.routeNetwork timeoutSlot of
    Just ms -> pure ms
    Nothing -> throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ "Unsupported network for slot conversion: " <> route.routeNetwork}
  let datumBytes = Htlc.mkDatumCbor hashBytes timeoutMs senderPkh receiverPkh
      downstreamFees = sum [h.hopFeeLovelace | h <- sorted, fromIntegral h.hopIndex > idx]
      minAdaFloor = case mRefScript of
        Just _ -> htlcLockMinAdaSharedLovelace
        Nothing -> htlcLockMinAdaInlineLovelace
      lockAmount = max minAdaFloor (route.routeAmountLovelace + downstreamFees)
      fee = recommendedLockFeeLovelace
      -- The lock tx needs: input ≥ lockAmount + fee + change-min-ada.
      -- We don't know the change-min-ada precisely, so add a 2 ADA
      -- cushion. The user's wallet should comfortably cover this.
      neededInput = lockAmount + fee + 2_000_000
  inputUtxo <- pickInputUtxo pool hop.hopHeadId req.walletAddress neededInput
  ppText <- fetchPP allowDirect pool headRow
  envOpt <- case mRefScript of
    Just _ -> pure Nothing
    Nothing -> case mScriptCbor of
      Nothing -> throwError $ err500{errBody = Aeson.encode $ ErrorResponse "HTLC script CBOR not configured; cannot inline ref script"}
      Just cbor -> pure $ Just (plutusEnvelopeJson cbor)
  let lockArgs =
        Tx.LockArgs
          { inputUtxo = inputUtxo.utxoTxHash <> "#" <> T.pack (show inputUtxo.utxoOutputIndex)
          , inputLovelace = inputUtxo.utxoLovelace
          , walletAddress = req.walletAddress
          , scriptAddress = scriptAddr
          , datumCborHex = Htlc.hexEncode datumBytes
          , sharedRefUtxo = mRefScript
          , lockAmount = lockAmount
          , validityUpperSlot = clampValidityUpper chainSlot timeoutSlot
          , requiredSignerPkhHex = Htlc.hexEncode senderPkh
          , feeLovelace = fee
          , protocolParamsJson = ppText
          , plutusEnvelope = envOpt
          }
  case Tx.buildLockTx lockArgs of
    Left err -> throwError $ err500{errBody = Aeson.encode $ ErrorResponse err}
    Right br -> pure br

-- | POST /api/v1/relay/payments/:routeId/hops/:hopIndex/claim-tx-cbor
handleClaimTxCbor
  :: Bool
  -> Pool
  -> TVar Int64
  -> Text
  -> Int
  -> BuildClaimTxRequest
  -> Handler Tx.BuildResult
handleClaimTxCbor allowDirect pool chainSlotVar rid idx req = do
  (_route, hop, _) <- loadHop pool rid idx
  chainSlot <- liftIO $ readTVarIO chainSlotVar
  htlcTxHash <- requireHtlcTx hop
  receiverPkh <- decodeAddrPkh "receiver" hop.hopReceiverAddress
  preimageBytes <- case Base16.decode (T.encodeUtf8 req.preimage) of
    Left e -> throwError $ err400{errBody = Aeson.encode $ ErrorResponse $ "preimage hex decode failed: " <> T.pack e}
    Right b -> pure b
  headRow <- loadHeadRow pool hop.hopHeadId
  refUtxo <- case headRow.headRefScriptUtxo of
    Nothing ->
      throwError $
        err409
          { errBody =
              Aeson.encode $
                ErrorResponse "head has no published HTLC ref-script UTxO; publish one first via /heads/{id}/publish-ref-script-tx-cbor and POST /heads/{id}/ref-script"
          }
    Just u -> pure u
  let redeemer = Htlc.mkClaimRedeemerCbor preimageBytes
      timeout = hop.hopTimeoutSlot
      fee = recommendedScriptFeeLovelace
      collateralNeeded = recommendedCollateralLovelace
      -- The HTLC UTxO carries 'lockAmount' lovelace from the lock
      -- side; we don't have that on the hop row directly. Read it
      -- from the indexed utxos table.
      htlcRef = (htlcTxHash, 0 :: Int32)
  -- Find the locked HTLC UTxO so we know its lovelace value
  mHtlcUtxo <- liftIO $ Db.findUtxoByRef pool hop.hopHeadId (fst htlcRef) (snd htlcRef)
  htlcLovelace <- case mHtlcUtxo of
    Just u -> pure u.utxoLovelace
    Nothing ->
      throwError $
        err409
          { errBody = Aeson.encode $ ErrorResponse $ "locked HTLC UTxO " <> htlcTxHash <> "#0 not found in head's snapshot"
          }
  collateralUtxo <- pickCollateralUtxo pool hop.hopHeadId req.walletAddress collateralNeeded htlcRef
  ppText <- fetchPP allowDirect pool headRow
  let claimArgs =
        Tx.ClaimArgs
          { htlcInputTxHash = htlcTxHash
          , htlcInputIndex = 0
          , refScriptUtxo = refUtxo
          , redeemerCborHex = Htlc.hexEncode redeemer
          , collateralUtxo = collateralUtxo.utxoTxHash <> "#" <> T.pack (show collateralUtxo.utxoOutputIndex)
          , collateralLovelace = collateralUtxo.utxoLovelace
          , totalCollateralLovelace = collateralNeeded
          , walletAddress = req.walletAddress
          , htlcOutputLovelace = htlcLovelace
          , validityUpperSlot = clampValidityUpper chainSlot timeout
          , requiredSignerPkhHex = Htlc.hexEncode receiverPkh
          , feeLovelace = fee
          , protocolParamsJson = ppText
          }
  case Tx.buildClaimTx claimArgs of
    Left err -> throwError $ err500{errBody = Aeson.encode $ ErrorResponse err}
    Right br -> pure br

-- | POST /api/v1/relay/payments/:routeId/hops/:hopIndex/refund-tx-cbor
handleRefundTxCbor
  :: Bool
  -> Pool
  -> TVar Int64
  -> Text
  -> Int
  -> BuildTxFromWalletRequest
  -> Handler Tx.BuildResult
handleRefundTxCbor allowDirect pool _chainSlotVar rid idx req = do
  (_route, hop, _) <- loadHop pool rid idx
  htlcTxHash <- requireHtlcTx hop
  senderPkh <- decodeAddrPkh "sender" hop.hopSenderAddress
  headRow <- loadHeadRow pool hop.hopHeadId
  refUtxo <- case headRow.headRefScriptUtxo of
    Nothing ->
      throwError $
        err409
          { errBody = Aeson.encode $ ErrorResponse "head has no published HTLC ref-script UTxO; publish one first"
          }
    Just u -> pure u
  let timeout = hop.hopTimeoutSlot
      fee = recommendedScriptFeeLovelace
      collateralNeeded = recommendedCollateralLovelace
      htlcRef = (htlcTxHash, 0 :: Int32)
  mHtlcUtxo <- liftIO $ Db.findUtxoByRef pool hop.hopHeadId (fst htlcRef) (snd htlcRef)
  htlcLovelace <- case mHtlcUtxo of
    Just u -> pure u.utxoLovelace
    Nothing -> throwError $ err409{errBody = Aeson.encode $ ErrorResponse $ "locked HTLC UTxO " <> htlcTxHash <> "#0 not found"}
  collateralUtxo <- pickCollateralUtxo pool hop.hopHeadId req.walletAddress collateralNeeded htlcRef
  ppText <- fetchPP allowDirect pool headRow
  let refundArgs =
        Tx.RefundArgs
          { htlcInputTxHash = htlcTxHash
          , htlcInputIndex = 0
          , refScriptUtxo = refUtxo
          , redeemerCborHex = Htlc.hexEncode Htlc.refundRedeemerCbor
          , collateralUtxo = collateralUtxo.utxoTxHash <> "#" <> T.pack (show collateralUtxo.utxoOutputIndex)
          , collateralLovelace = collateralUtxo.utxoLovelace
          , totalCollateralLovelace = collateralNeeded
          , walletAddress = req.walletAddress
          , htlcOutputLovelace = htlcLovelace
          , validityLowerSlot = timeout + htlcSafetyMarginSlots
          , requiredSignerPkhHex = Htlc.hexEncode senderPkh
          , feeLovelace = fee
          , protocolParamsJson = ppText
          }
  case Tx.buildRefundTx refundArgs of
    Left err -> throwError $ err500{errBody = Aeson.encode $ ErrorResponse err}
    Right br -> pure br

-- | POST /api/v1/heads/:headId/publish-ref-script-tx-cbor
handlePublishRefTxCbor
  :: Bool
  -> Pool
  -> Maybe Text -- htlcScriptCbor
  -> Text
  -> BuildTxFromWalletRequest
  -> Handler Tx.BuildResult
handlePublishRefTxCbor allowDirect pool mScriptCbor hid req = do
  scriptCbor <- case mScriptCbor of
    Nothing -> throwError $ err500{errBody = Aeson.encode $ ErrorResponse "HTLC script CBOR not configured"}
    Just c -> pure c
  headRow <- loadHeadRow pool hid
  let refOutVal = 6_000_000 :: Int64
      fee = 300_000 :: Int64
      neededInput = refOutVal + fee + 2_000_000
  inputUtxo <- pickInputUtxo pool hid req.walletAddress neededInput
  ppText <- fetchPP allowDirect pool headRow
  let pubArgs =
        Tx.PublishRefArgs
          { inputUtxo = inputUtxo.utxoTxHash <> "#" <> T.pack (show inputUtxo.utxoOutputIndex)
          , inputLovelace = inputUtxo.utxoLovelace
          , walletAddress = req.walletAddress
          , refOutputLovelace = refOutVal
          , feeLovelace = fee
          , protocolParamsJson = ppText
          , plutusEnvelope = plutusEnvelopeJson scriptCbor
          }
  case Tx.buildPublishRefTx pubArgs of
    Left err -> throwError $ err500{errBody = Aeson.encode $ ErrorResponse err}
    Right br -> pure br

-- | POST /api/v1/heads/:headId/submit
-- Prefers the agent command queue: the head's agent picks the signed tx
-- up on its next poll, submits it via its /local/ hydra-node, and reports
-- the verdict back — the registry never dials the user's node. Falls back
-- to the legacy direct-WS path only when no live agent exists.
handleSubmitTx :: Bool -> Pool -> CommandWaiters -> Text -> SubmitTxRequest -> Handler Submit.SubmitResult
handleSubmitTx allowDirect pool waiters hid req = do
  mAgent <- liftIO $ Db.lookupActiveAgentForHead pool hid agentLivenessSeconds
  case mAgent of
    Just _ -> do
      cmdId <- liftIO $ T.replace "-" "" . T.pack . UUID.toString <$> UUID.nextRandom
      liftIO $ Db.insertAgentCommand pool cmdId hid "submit_tx" req.signedCborHex
      mResult <- liftIO $ awaitCommand waiters cmdId submitWaitSeconds
      case mResult of
        Just r -> pure r
        Nothing -> do
          liftIO $ Db.failAgentCommand pool cmdId "agent did not report a result in time"
          pure Submit.SubmitTimeout
    Nothing
      | allowDirect -> do
          headRow <- loadHeadRow pool hid
          liftIO $ Submit.submitToHead headRow.headHost (fromIntegral headRow.headPort) req.signedCborHex
      | otherwise -> do
          _ <- loadHeadRow pool hid -- 404 for unknown heads
          throwError $
            err503
              { errBody =
                  Aeson.encode $
                    ErrorResponse "no live agent for this head and direct node access is disabled; run hydra-registry-agent next to the hydra-node"
              }

-- | An agent counts as live when it polled (or pushed an event) this
-- recently; the poll interval is a few seconds, so 90s is generous.
agentLivenessSeconds :: Int
agentLivenessSeconds = 90

-- | How long the submit handler waits for the agent's verdict: one poll
-- interval + local WS submit (15s budget) + reporting, with headroom.
submitWaitSeconds :: Int
submitWaitSeconds = 30

-- | Reject handlers that would dial a user's hydra-node when
-- HYDRA_DIRECT_WS is off (the production default).
requireDirectWs :: Bool -> Handler ()
requireDirectWs allowDirect =
  unless allowDirect $
    throwError $
      err403
        { errBody =
            Aeson.encode $
              ErrorResponse "direct hydra-node access is disabled on this registry; use the agent push model (hydra-registry-agent)"
        }

-- | Fetch protocol params for a head and translate any error into
-- an HTTP error so the handlers above don't have to repeat themselves.
fetchPP :: Bool -> Pool -> Head Identity -> Handler Text
fetchPP allowDirect pool h = do
  -- Prefer agent-pushed parameters: the registry should never need to
  -- reach a user's hydra-node (its API is unauthenticated).
  mStored <- liftIO $ Db.getHeadProtocolParams pool h.headId
  case mStored of
    Just v -> pure $ T.decodeUtf8 $ BSL.toStrict $ Aeson.encode v
    Nothing
      | allowDirect -> do
          res <- liftIO $ fetchProtocolParams h.headHost (fromIntegral h.headPort)
          case res of
            Left e -> throwError $ err502{errBody = Aeson.encode $ ErrorResponse e}
            Right t -> pure t
      | otherwise ->
          throwError $
            err503
              { errBody =
                  Aeson.encode $
                    ErrorResponse "protocol parameters not available: the head's agent has not pushed them and direct node access is disabled"
              }

-- ─── existing helpers ────────────────────────────────────────────────

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

-- ─── User profile handlers ───

-- | GET /api/v1/users/:walletAddress/keyhash
handleGetUserKeyHash :: Pool -> Text -> Handler UserKeyHashResponse
handleGetUserKeyHash pool walletAddr = do
  kh <- liftIO $ Db.getUserKeyHash pool walletAddr
  pure $ UserKeyHashResponse kh

-- | PUT /api/v1/users/:walletAddress/keyhash
handleSetUserKeyHash :: Pool -> Text -> SetKeyHashRequest -> Handler UserKeyHashResponse
handleSetUserKeyHash pool walletAddr req = do
  liftIO $ Db.setUserKeyHash pool walletAddr req.keyHash
  pure $ UserKeyHashResponse (Just req.keyHash)

-- ─── Agent handlers ───

hashSecret :: Text -> Text
hashSecret t = T.pack $ show (hash (T.encodeUtf8 t) :: Digest SHA256)

-- | POST /api/v1/agent/register
-- Issues a per-agent secret key. Empty allowedHashes list = dev mode (any hash accepted).
handleAgentRegister :: Pool -> [Text] -> AgentRegisterRequest -> Handler AgentRegisterResponse
handleAgentRegister pool allowedHashes req = do
  when (not (null allowedHashes) && req.binaryHash `notElem` allowedHashes) $
    throwError err403{errBody = Aeson.encode $ ErrorResponse "binary hash not in allowed list"}
  let (wsHost, wsPort) = parseWsHostPort req.wsUrl
  agentId' <- liftIO $ T.replace "-" "" . T.pack . UUID.toString <$> UUID.nextRandom
  secretKey <- liftIO $ T.replace "-" "" . T.pack . UUID.toString <$> UUID.nextRandom
  let secretHash = hashSecret secretKey
  liftIO $ Db.insertAgentRegistration pool agentId' req.headId secretHash req.binaryHash wsHost wsPort
  pure $ AgentRegisterResponse{agentId = agentId', secretKey}

parseWsHostPort :: Text -> (Text, Int)
parseWsHostPort url =
  let stripped = fromMaybe (fromMaybe url (T.stripPrefix "ws://" url)) (T.stripPrefix "wss://" url)
      hostPort = T.takeWhile (/= '/') stripped
      (host', portPart) = T.breakOn ":" hostPort
      port' = case T.stripPrefix ":" portPart of
        Just p -> fromMaybe 4001 (readMaybe @Int (T.unpack p))
        Nothing -> 4001
  in (host', port')

-- | POST /api/v1/agent/events
-- Accepts a Hydra event pushed by the CLI agent.
handleAgentEvent
  :: Pool
  -> [Text]
  -> TQueue HydraEvent
  -> Maybe Text
  -> Maybe Text
  -> AgentEventRequest
  -> Handler MessageResponse
handleAgentEvent pool allowedHashes eventQueue mAuthHeader mBinaryHashHeader req = do
  agent <- requireAgent pool allowedHashes mAuthHeader mBinaryHashHeader
  now <- liftIO getCurrentTime
  liftIO $ Db.updateAgentLastSeen pool agent.agentId now
  hydraEvent <- case Hydra.Client.parseHydraMessage req.event of
    Nothing -> throwError err400{errBody = Aeson.encode $ ErrorResponse "could not parse event as HydraEvent"}
    Just e -> pure e
  -- On the first Open Greetings from this agent, create the head row.
  -- We defer this until we know the head is actually Open so idle heads
  -- never appear in the registry.
  case hydraEvent of
    HeadGreetings{greeterHeadId, greeterHeadStatus} | greeterHeadStatus == "Open" ->
      liftIO $ Db.upsertHead pool greeterHeadId
        agent.agentWsHost
        (fromIntegral agent.agentWsPort)
        greeterHeadStatus
        Nothing
    _ -> pure ()
  liftIO $ atomically $ writeTQueue eventQueue hydraEvent
  pure $ MessageResponse "event accepted"

-- | Authenticate an agent request: Bearer secret + binary-hash header,
-- checked against the registration row and the optional allowlist.
requireAgent :: Pool -> [Text] -> Maybe Text -> Maybe Text -> Handler (AgentRegistration Identity)
requireAgent pool allowedHashes mAuthHeader mBinaryHashHeader = do
  authToken <- case mAuthHeader of
    Nothing -> throwError err401{errBody = Aeson.encode $ ErrorResponse "Authorization header required"}
    Just h | T.isPrefixOf "Bearer " h -> pure $ T.drop 7 h
    Just _ -> throwError err401{errBody = Aeson.encode $ ErrorResponse "Authorization must be Bearer token"}
  binaryHashHdr <- case mBinaryHashHeader of
    Nothing -> throwError err400{errBody = Aeson.encode $ ErrorResponse "X-Agent-Binary-Hash header required"}
    Just h -> pure h
  let secretHash = hashSecret authToken
  mAgent <- liftIO $ Db.lookupAgentBySecretHash pool secretHash
  agent <- case mAgent of
    Nothing -> throwError err401{errBody = Aeson.encode $ ErrorResponse "invalid or unknown agent secret"}
    Just a -> pure a
  when (agent.agentBinaryHash /= binaryHashHdr) $
    throwError err403{errBody = Aeson.encode $ ErrorResponse "binary hash mismatch"}
  when (not (null allowedHashes) && binaryHashHdr `notElem` allowedHashes) $
    throwError err403{errBody = Aeson.encode $ ErrorResponse "binary hash not in allowed list"}
  pure agent

-- | PUT /api/v1/agent/heads/:headId/protocol-parameters
-- The agent pushes its local node's protocol parameters so server-side
-- tx building never needs to reach the user's hydra-node.
handleAgentPushPParams :: Pool -> [Text] -> Text -> Maybe Text -> Maybe Text -> Aeson.Value -> Handler MessageResponse
handleAgentPushPParams pool allowedHashes headId' mAuthHeader mBinaryHashHeader params = do
  agent <- requireAgent pool allowedHashes mAuthHeader mBinaryHashHeader
  when (agent.agentHeadId /= headId') $
    throwError err403{errBody = Aeson.encode $ ErrorResponse "agent is not registered for this head"}
  liftIO $ Db.setHeadProtocolParams pool headId' params
  pure $ MessageResponse "protocol parameters stored"

-- | POST /api/v1/agent/commands/poll
-- Hands all pending commands for the agent's head to the agent and
-- marks them delivered. Also refreshes the agent's liveness timestamp —
-- the submit handler only queues commands for heads with a live agent.
handleAgentPollCommands :: Pool -> [Text] -> Maybe Text -> Maybe Text -> Handler [AgentCommandInfo]
handleAgentPollCommands pool allowedHashes mAuthHeader mBinaryHashHeader = do
  agent <- requireAgent pool allowedHashes mAuthHeader mBinaryHashHeader
  now <- liftIO getCurrentTime
  liftIO $ Db.updateAgentLastSeen pool agent.agentId now
  cmds <- liftIO $ Db.claimPendingCommands pool agent.agentHeadId
  pure
    [ AgentCommandInfo{commandId = c.commandId, kind = c.commandKind, payload = c.commandPayload}
    | c <- cmds
    ]

-- | POST /api/v1/agent/commands/:commandId/result
-- The agent reports the outcome of a command it executed against its
-- local node; wakes the submit handler waiting on it (if still there).
handleAgentCommandResult :: Pool -> [Text] -> CommandWaiters -> Text -> Maybe Text -> Maybe Text -> Submit.SubmitResult -> Handler MessageResponse
handleAgentCommandResult pool allowedHashes waiters cmdId mAuthHeader mBinaryHashHeader result = do
  agent <- requireAgent pool allowedHashes mAuthHeader mBinaryHashHeader
  mCmd <- liftIO $ Db.getAgentCommand pool cmdId
  cmd <- case mCmd of
    Nothing -> throwError err404{errBody = Aeson.encode $ ErrorResponse "unknown command"}
    Just c -> pure c
  when (cmd.commandHeadId /= agent.agentHeadId) $
    throwError err403{errBody = Aeson.encode $ ErrorResponse "command belongs to a different head"}
  liftIO $ Db.completeAgentCommand pool cmdId (Aeson.toJSON result)
  _ <- liftIO $ resolveCommand waiters cmdId result
  pure $ MessageResponse "result recorded"

-- ─── Claim ownership ───

-- | POST /api/v1/heads/:headId/claim-ownership
-- The caller proves hydra-node access by having deposited a UTxO from their wallet into
-- the head. We check the L2 snapshot for that address; if found, extract the payment key
-- hash from the bech32 address and store it as the user's key hash.
handleClaimOwnership :: Pool -> Text -> ClaimOwnershipRequest -> Handler ClaimOwnershipResponse
handleClaimOwnership pool headId' req = do
  mHead <- liftIO $ Db.getHead pool headId'
  _ <- case mHead of
    Nothing -> throwError err404{errBody = Aeson.encode $ ErrorResponse "head not found"}
    Just h -> pure h
  utxos <- liftIO $ Db.getUtxosByAddressAndHead pool headId' req.walletAddress
  when (null utxos) $
    throwError err404
      { errBody = Aeson.encode $ ErrorResponse
          "No UTxO from this wallet found in the head snapshot. Deposit a UTxO from your wallet into this head first."
      }
  pkh <- case extractPkhFromAddress req.walletAddress of
    Left e -> throwError err400{errBody = Aeson.encode $ ErrorResponse e}
    Right p -> pure p
  liftIO $ Db.setUserKeyHash pool req.walletAddress pkh
  liftIO $ Db.setHeadRegisteredBy pool headId' req.walletAddress
  pure $ ClaimOwnershipResponse{verified = True, keyHash = pkh}
