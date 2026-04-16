module Api where

import Api.Types
import Api.Validation (validateAddress)
import Cache (Cache, insertCache, lookupCache)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Db qualified
import Db.Schema (Head (..), Utxo (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraEvent (..))
import Indexer qualified
import Logging (Logger)
import Metrics (Metrics, renderMetrics)
import Servant

-- | Full API type
type API =
  -- GET / — root endpoint with version and docs link
  Get '[JSON] RootResponse
    :<|> "health" :> Get '[JSON] HealthResponse
    :<|> "heads" :> "register" :> ReqBody '[JSON] RegisterHead :> Post '[JSON] RegisterHeadResponse
    :<|> "heads" :> QueryParam "count" Int :> QueryParam "page" Int :> Get '[JSON] [HeadInfo]
    :<|> "heads" :> Capture "headId" Text :> Get '[JSON] HeadDetailResponse
    :<|> "heads" :> Capture "headId" Text :> "addresses" :> Get '[JSON] [Text]
    :<|> "heads" :> Capture "headId" Text :> "addresses" :> Capture "address" Text :> "balance" :> Get '[JSON] BalanceResponse
    :<|> "heads" :> Capture "headId" Text :> "addresses" :> Capture "address" Text :> "utxos" :> Get '[JSON] [UtxoResponse]
    :<|> "addresses" :> Capture "address" Text :> "utxos"
          :> QueryParam "count" Int :> QueryParam "page" Int :> QueryParam "order" Text
          :> Get '[JSON] [UtxoResponse]
    :<|> "admin" :> "heads" :> Capture "headId" Text :> Delete '[JSON] NoContent
    :<|> "metrics" :> Get '[PlainText] Text

api :: Proxy API
api = Proxy

-- | Application environment shared across handlers
data AppEnv = AppEnv
  { pool :: Pool
  , eventQueue :: TQueue HydraEvent
  , logger :: Logger
  , metrics :: Metrics
  , addressCache :: Cache [UtxoResponse]
  }

-- | Create the Servant server
server :: AppEnv -> Server API
server env =
  handleRoot
    :<|> handleHealth env.pool
    :<|> handleRegister env.logger env.pool env.eventQueue
    :<|> handleListHeads env.pool
    :<|> handleHeadDetail env.pool
    :<|> handleHeadAddresses env.pool
    :<|> handleAddressBalance env.pool
    :<|> handleHeadUtxos env.pool
    :<|> handleAddressUtxos env.pool env.addressCache
    :<|> handleAdminDeleteHead env.pool
    :<|> handleMetrics env.metrics

-- | GET /
handleRoot :: Handler RootResponse
handleRoot =
  pure
    RootResponse
      { apiVersion = "0.1.0"
      , description = "Hydra Registry API — query L2 UTxO state across Hydra heads"
      }

-- | GET /health
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

-- | POST /heads/register
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

-- | GET /heads (with optional pagination)
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

-- | GET /heads/:headId
handleHeadDetail :: Pool -> Text -> Handler HeadDetailResponse
handleHeadDetail pool hid = do
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just h -> do
      utxoCount <- liftIO $ Db.countUtxosForHead pool hid
      pure
        HeadDetailResponse
          { headId = h.headId
          , host = h.headHost
          , port = fromIntegral h.headPort
          , status = h.headStatus
          , utxoCount = utxoCount
          , registeredAt = h.createdAt
          , lastSeenAt = h.lastMessageAt
          }

-- | GET /heads/:headId/addresses
handleHeadAddresses :: Pool -> Text -> Handler [Text]
handleHeadAddresses pool hid = do
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ ->
      liftIO $ Db.getAddressesForHead pool hid

-- | GET /heads/:headId/addresses/:address/balance
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

-- | GET /heads/:headId/addresses/:address/utxos
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

-- | DELETE /admin/heads/:headId
handleAdminDeleteHead :: Pool -> Text -> Handler NoContent
handleAdminDeleteHead pool hid = do
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ -> do
      liftIO $ Db.deleteHead pool hid
      pure NoContent

-- | GET /metrics
handleMetrics :: Metrics -> Handler Text
handleMetrics = liftIO . renderMetrics

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
