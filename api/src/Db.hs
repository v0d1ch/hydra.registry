module Db where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Db.Schema
import Hasql.Connection.Setting qualified as Hasql.Conn
import Hasql.Connection.Setting.Connection qualified as Hasql.ConnStr
import Hasql.Pool (Pool)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool.Config
import Hasql.Session qualified as Session
import Hydra.Client (HydraUtxoEntry (..))
import Rel8 hiding (null)

-- | Create the database pool
createPool :: Text -> IO Pool
createPool connStr =
  Pool.acquire $
    Pool.Config.settings
      [ Pool.Config.staticConnectionSettings
          [Hasql.Conn.connection (Hasql.ConnStr.string connStr)]
      ]

-- | Run a Hasql session via the pool, throwing on error
runSession :: Pool -> Session.Session a -> IO a
runSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> fail $ "Database error: " <> show err
    Right a -> pure a

-- | Initialize database schema
initDb :: Pool -> IO ()
initDb pool =
  runSession pool $
    Session.sql
      "CREATE TABLE IF NOT EXISTS heads (\
      \  head_id TEXT PRIMARY KEY,\
      \  host TEXT NOT NULL,\
      \  port INTEGER NOT NULL,\
      \  status TEXT NOT NULL DEFAULT 'idle',\
      \  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
      \  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
      \  last_message_at TIMESTAMPTZ\
      \);\
      \CREATE TABLE IF NOT EXISTS utxos (\
      \  tx_hash TEXT NOT NULL,\
      \  output_index INTEGER NOT NULL,\
      \  head_id TEXT NOT NULL REFERENCES heads(head_id) ON DELETE CASCADE,\
      \  address TEXT NOT NULL,\
      \  lovelace BIGINT NOT NULL DEFAULT 0,\
      \  assets JSONB NOT NULL DEFAULT '{}',\
      \  datum_hash TEXT,\
      \  inline_datum JSONB,\
      \  reference_script_hash TEXT,\
      \  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
      \  PRIMARY KEY (tx_hash, output_index, head_id)\
      \);\
      \CREATE INDEX IF NOT EXISTS idx_utxos_address ON utxos (address);\
      \CREATE INDEX IF NOT EXISTS idx_utxos_head_id ON utxos (head_id);"

-- | Insert a new head or update on conflict
upsertHead :: Pool -> Text -> Text -> Int -> Text -> IO ()
upsertHead pool hid hostAddr portNum status' = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.insert
          Insert
            { into = headSchema
            , rows =
                Rel8.values
                  [ Head
                      { headId = lit hid
                      , headHost = lit hostAddr
                      , headPort = lit (fromIntegral @Int @Int32 portNum)
                      , headStatus = lit status'
                      , createdAt = lit now
                      , updatedAt = lit now
                      , lastMessageAt = lit (Just now)
                      }
                  ]
            , onConflict =
                DoUpdate
                  Upsert
                    { index = (.headId)
                    , predicate = Nothing
                    , set = \new _old ->
                        new{updatedAt = lit now, lastMessageAt = lit (Just now)}
                    , updateWhere = \_ _ -> lit True
                    }
            , returning = NoReturning
            }

-- | Update head status
updateHeadStatus :: Pool -> Text -> Text -> IO ()
updateHeadStatus pool hid newStatus = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = headSchema
            , from = pure ()
            , set = \_ row ->
                row
                  { headStatus = lit newStatus
                  , updatedAt = lit now
                  , lastMessageAt = lit (Just now)
                  }
            , updateWhere = \_ row -> row.headId ==. lit hid
            , returning = NoReturning
            }

-- | Update last message timestamp for a head (health tracking)
updateLastMessageAt :: Pool -> Text -> IO ()
updateLastMessageAt pool hid = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = headSchema
            , from = pure ()
            , set = \_ row -> row{lastMessageAt = lit (Just now)}
            , updateWhere = \_ row -> row.headId ==. lit hid
            , returning = NoReturning
            }

-- | Get all registered heads
getAllHeads :: Pool -> IO [Head Identity]
getAllHeads pool =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.each headSchema

-- | Get all registered heads with pagination
getAllHeadsPaginated :: Pool -> Int -> Int -> IO [Head Identity]
getAllHeadsPaginated pool pageSize page =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.limit (fromIntegral pageSize) $
            Rel8.offset (fromIntegral $ (page - 1) * pageSize) $
              Rel8.each headSchema

-- | Get a specific head by ID
getHead :: Pool -> Text -> IO (Maybe (Head Identity))
getHead pool hid =
  runSession pool $ do
    rows <-
      Session.statement () $
        Rel8.run $
          Rel8.select $ do
            h <- Rel8.each headSchema
            Rel8.where_ (h.headId ==. lit hid)
            pure h
    pure $ case rows of
      [] -> Nothing
      (x : _) -> Just x

-- | Count UTxOs for a specific head
countUtxosForHead :: Pool -> Text -> IO Int
countUtxosForHead pool hid = do
  utxos <- runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $ do
          u <- Rel8.each utxoSchema
          Rel8.where_ (u.utxoHeadId ==. lit hid)
          pure u
  pure $ length utxos

-- | Get distinct addresses for a head
getAddressesForHead :: Pool -> Text -> IO [Text]
getAddressesForHead pool hid = do
  utxos <- runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $ do
          u <- Rel8.each utxoSchema
          Rel8.where_ (u.utxoHeadId ==. lit hid)
          pure u.utxoAddress
  pure $ deduplicate utxos
 where
  deduplicate = Map.keys . Map.fromList . map (\a -> (a, ()))

-- | Get aggregated balance for an address in a head
getBalanceForAddressInHead :: Pool -> Text -> Text -> IO (Int64, Map.Map Text (Map.Map Text Integer))
getBalanceForAddressInHead pool hid addr = do
  utxos <- getUtxosByAddressAndHead pool hid addr
  let totalLovelace = Prelude.sum $ map (.utxoLovelace) utxos
      mergedAssets = mergeAllAssets $ map (.utxoAssets) utxos
  pure (totalLovelace, mergedAssets)
 where
  mergeAllAssets :: [Aeson.Value] -> Map.Map Text (Map.Map Text Integer)
  mergeAllAssets = foldr mergeOne Map.empty

  mergeOne :: Aeson.Value -> Map.Map Text (Map.Map Text Integer) -> Map.Map Text (Map.Map Text Integer)
  mergeOne (Aeson.Object obj) acc =
    foldr
      ( \(k, v) m -> case v of
          Aeson.Object assets ->
            let policyId = Key.toText k
                assetMap =
                  Map.fromList
                    [ (Key.toText ak, round n)
                    | (ak, Aeson.Number n) <- KM.toList assets
                    ]
             in Map.insertWith (Map.unionWith (+)) policyId assetMap m
          _ -> m
      )
      acc
      (KM.toList obj)
  mergeOne _ acc = acc

-- | Replace all UTxOs for a head with new snapshot data
replaceUtxos :: Pool -> Text -> [HydraUtxoEntry] -> IO ()
replaceUtxos pool hid entries = do
  now <- getCurrentTime
  runSession pool $ do
    -- Delete existing UTxOs for this head
    Session.statement () $
      Rel8.run_ $
        Rel8.delete
          Delete
            { from = utxoSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.utxoHeadId ==. lit hid
            , returning = NoReturning
            }
    -- Insert new UTxOs
    case entries of
      [] -> pure ()
      _ ->
        Session.statement () $
          Rel8.run_ $
            Rel8.insert
              Insert
                { into = utxoSchema
                , rows = Rel8.values $ map (toUtxoRow now) entries
                , onConflict = DoNothing
                , returning = NoReturning
                }
 where
  toUtxoRow :: UTCTime -> HydraUtxoEntry -> Utxo Expr
  toUtxoRow now entry =
    Utxo
      { utxoTxHash = lit entry.txHash
      , utxoOutputIndex = lit (fromIntegral @Int @Int32 entry.outputIndex)
      , utxoHeadId = lit hid
      , utxoAddress = lit entry.address
      , utxoLovelace = lit (fromIntegral @Integer @Int64 entry.lovelace)
      , utxoAssets = lit (assetsToJson entry.nativeAssets)
      , utxoDatumHash = lit entry.datumHash
      , utxoInlineDatum = lit entry.inlineDatum
      , utxoReferenceScriptHash = lit Nothing
      , utxoUpdatedAt = lit now
      }

  assetsToJson :: Map.Map Text (Map.Map Text Integer) -> Aeson.Value
  assetsToJson = Aeson.toJSON

-- | Get UTxOs for an address in a specific head
getUtxosByAddressAndHead :: Pool -> Text -> Text -> IO [Utxo Identity]
getUtxosByAddressAndHead pool hid addr =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $ do
          u <- Rel8.each utxoSchema
          Rel8.where_ (u.utxoHeadId ==. lit hid)
          Rel8.where_ (u.utxoAddress ==. lit addr)
          pure u

-- | Get UTxOs for an address across all heads, grouped by head
getUtxosByAddress :: Pool -> Text -> IO [(Head Identity, [Utxo Identity])]
getUtxosByAddress pool addr = do
  heads <- getAllHeads pool
  results <- mapM getForHead heads
  pure [(h, us) | (h, us) <- results, not (Prelude.null us)]
 where
  getForHead h = do
    us <- getUtxosByAddressAndHead pool h.headId addr
    pure (h, us)

-- | Delete all UTxOs for a head
deleteUtxosForHead :: Pool -> Text -> IO ()
deleteUtxosForHead pool hid =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.delete
          Delete
            { from = utxoSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.utxoHeadId ==. lit hid
            , returning = NoReturning
            }

-- | Delete a head and its UTxOs (admin deregistration)
deleteHead :: Pool -> Text -> IO ()
deleteHead pool hid = do
  -- UTxOs are deleted by ON DELETE CASCADE
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.delete
          Delete
            { from = headSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.headId ==. lit hid
            , returning = NoReturning
            }

-- | Check database connectivity
checkDbConnectivity :: Pool -> IO Bool
checkDbConnectivity pool = do
  result <- Pool.use pool $ Session.sql "SELECT 1"
  pure $ case result of
    Left _ -> False
    Right _ -> True
