module Api where

import Api.Types
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Functor.Identity (Identity)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text qualified as T
import Db qualified
import Db.Schema (Head (..), Utxo (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraEvent (..))
import Indexer qualified
import Servant

-- | Full API type
type API =
  "health" :> Get '[JSON] HealthResponse
    :<|> "heads" :> "register" :> ReqBody '[JSON] RegisterHead :> Post '[JSON] RegisterHeadResponse
    :<|> "heads" :> Get '[JSON] [HeadInfo]
    :<|> "heads" :> Capture "headId" Text :> "addresses" :> Capture "address" Text :> "utxos" :> Get '[JSON] [UtxoResponse]
    :<|> "addresses" :> Capture "address" Text :> "utxos" :> Get '[JSON] [HeadUtxoResponse]

api :: Proxy API
api = Proxy

-- | Create the Servant server
server :: Pool -> TQueue HydraEvent -> Server API
server pool eventQueue =
  handleHealth pool
    :<|> handleRegister pool eventQueue
    :<|> handleListHeads pool
    :<|> handleHeadUtxos pool
    :<|> handleAddressUtxos pool

-- | GET /health
handleHealth :: Pool -> Handler HealthResponse
handleHealth pool = do
  heads <- liftIO $ Db.getAllHeads pool
  pure $ HealthResponse "ok" (length heads)

-- | POST /heads/register
handleRegister :: Pool -> TQueue HydraEvent -> RegisterHead -> Handler RegisterHeadResponse
handleRegister pool eventQueue req = do
  result <- liftIO $ Indexer.registerHead pool eventQueue req.host req.port
  case result of
    Left err ->
      throwError $ err400{errBody = Aeson.encode $ ErrorResponse err}
    Right HeadGreetings{greeterHeadId} ->
      pure $ RegisterHeadResponse greeterHeadId "connected"
    Right _ ->
      throwError $ err500{errBody = Aeson.encode $ ErrorResponse "Unexpected response"}

-- | GET /heads
handleListHeads :: Pool -> Handler [HeadInfo]
handleListHeads pool = do
  heads <- liftIO $ Db.getAllHeads pool
  pure $ map toHeadInfo heads
 where
  toHeadInfo h =
    HeadInfo
      { headId = h.headId
      , host = h.headHost
      , port = fromIntegral h.headPort
      , status = h.headStatus
      }

-- | GET /heads/:headId/addresses/:address/utxos
handleHeadUtxos :: Pool -> Text -> Text -> Handler [UtxoResponse]
handleHeadUtxos pool hid addr = do
  mHead <- liftIO $ Db.getHead pool hid
  case mHead of
    Nothing ->
      throwError $ err404{errBody = Aeson.encode $ ErrorResponse "Head not found"}
    Just _ -> do
      utxos <- liftIO $ Db.getUtxosByAddressAndHead pool hid addr
      pure $ map utxoToResponse utxos

-- | GET /addresses/:address/utxos
handleAddressUtxos :: Pool -> Text -> Handler [HeadUtxoResponse]
handleAddressUtxos pool addr = do
  results <- liftIO $ Db.getUtxosByAddress pool addr
  pure $ map toHeadUtxoResponse results
 where
  toHeadUtxoResponse (h, utxos) =
    HeadUtxoResponse
      { head_id = h.headId
      , head_status = h.headStatus
      , utxos = map utxoToResponse utxos
      }

-- | Convert a DB UTxO row to the Blockfrost-compatible response format
utxoToResponse :: Utxo Identity -> UtxoResponse
utxoToResponse u =
  UtxoResponse
    { address = u.utxoAddress
    , tx_hash = u.utxoTxHash
    , output_index = fromIntegral u.utxoOutputIndex
    , amount = lovelaceAmount : nativeTokenAmounts
    , data_hash = u.utxoDatumHash
    , inline_datum = u.utxoInlineDatum
    , reference_script_hash = u.utxoReferenceScriptHash
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
