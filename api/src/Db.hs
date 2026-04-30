module Db where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Db.Schema
import Hasql.Connection.Setting qualified as Hasql.Conn
import Hasql.Connection.Setting.Connection qualified as Hasql.ConnStr
import Hasql.Pool (Pool)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool.Config
import Hasql.Session qualified as Session
import Hydra.Client (HydraUtxoEntry (..), extractReferenceScriptHash)
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
      \  snapshot_number INTEGER NOT NULL DEFAULT 0,\
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
      \CREATE INDEX IF NOT EXISTS idx_utxos_head_id ON utxos (head_id);\
      \CREATE TABLE IF NOT EXISTS explorer_heads (\
      \  head_id TEXT PRIMARY KEY,\
      \  network TEXT NOT NULL,\
      \  network_magic INTEGER NOT NULL,\
      \  version TEXT NOT NULL,\
      \  status TEXT NOT NULL,\
      \  contestation_period INTEGER,\
      \  contestations INTEGER,\
      \  snapshot_number INTEGER,\
      \  contestation_deadline TEXT,\
      \  point JSONB,\
      \  block_no BIGINT,\
      \  members JSONB,\
      \  seed_tx_in TEXT,\
      \  first_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
      \  last_updated_at TIMESTAMPTZ NOT NULL DEFAULT now()\
      \);\
      \CREATE INDEX IF NOT EXISTS idx_explorer_heads_status ON explorer_heads (status);\
      \CREATE INDEX IF NOT EXISTS idx_explorer_heads_network ON explorer_heads (network);\
      \ALTER TABLE heads ADD COLUMN IF NOT EXISTS is_bridge BOOLEAN NOT NULL DEFAULT false;\
      \ALTER TABLE heads ADD COLUMN IF NOT EXISTS bridge_fee_lovelace BIGINT;\
      \ALTER TABLE heads ADD COLUMN IF NOT EXISTS ref_script_utxo TEXT;\
      \CREATE TABLE IF NOT EXISTS head_participants (\
      \  head_id TEXT NOT NULL,\
      \  address TEXT NOT NULL,\
      \  vkey TEXT,\
      \  on_chain_id TEXT,\
      \  committed_lovelace BIGINT NOT NULL DEFAULT 0,\
      \  committed_tx_ref TEXT,\
      \  PRIMARY KEY (head_id, address)\
      \);\
      \CREATE INDEX IF NOT EXISTS idx_head_participants_address ON head_participants (address);\
      \CREATE TABLE IF NOT EXISTS invoices (\
      \  invoice_id TEXT PRIMARY KEY,\
      \  receiver_on_chain_id TEXT NOT NULL DEFAULT '',\
      \  payment_hash TEXT NOT NULL,\
      \  amount_lovelace BIGINT NOT NULL,\
      \  memo TEXT,\
      \  status TEXT NOT NULL DEFAULT 'pending',\
      \  expires_at TIMESTAMPTZ NOT NULL,\
      \  created_at TIMESTAMPTZ NOT NULL DEFAULT now()\
      \);\
      \ALTER TABLE invoices ADD COLUMN IF NOT EXISTS receiver_on_chain_id TEXT NOT NULL DEFAULT '';\
      \ALTER TABLE invoices DROP COLUMN IF EXISTS receiver_address;\
      \CREATE TABLE IF NOT EXISTS payment_routes (\
      \  route_id TEXT PRIMARY KEY,\
      \  invoice_id TEXT NOT NULL,\
      \  sender_address TEXT NOT NULL,\
      \  receiver_address TEXT NOT NULL,\
      \  amount_lovelace BIGINT NOT NULL,\
      \  status TEXT NOT NULL DEFAULT 'requested',\
      \  route_path JSONB NOT NULL DEFAULT '[]',\
      \  total_fee BIGINT NOT NULL DEFAULT 0,\
      \  network TEXT NOT NULL DEFAULT 'Testnet',\
      \  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
      \  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()\
      \);\
      \CREATE TABLE IF NOT EXISTS route_hops (\
      \  hop_id TEXT PRIMARY KEY,\
      \  route_id TEXT NOT NULL,\
      \  hop_index INTEGER NOT NULL,\
      \  head_id TEXT NOT NULL,\
      \  bridge_address TEXT NOT NULL,\
      \  sender_address TEXT NOT NULL,\
      \  receiver_address TEXT NOT NULL,\
      \  htlc_status TEXT NOT NULL DEFAULT 'pending',\
      \  htlc_tx_hash TEXT,\
      \  secret_hash TEXT NOT NULL,\
      \  preimage TEXT,\
      \  timeout_slot BIGINT NOT NULL,\
      \  fee_lovelace BIGINT NOT NULL DEFAULT 0,\
      \  locked_at TIMESTAMPTZ,\
      \  claimed_at TIMESTAMPTZ\
      \);\
      \CREATE INDEX IF NOT EXISTS idx_route_hops_route_id ON route_hops (route_id);\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS sender_address TEXT NOT NULL DEFAULT '';\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS receiver_address TEXT NOT NULL DEFAULT '';\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS htlc_status TEXT NOT NULL DEFAULT 'pending';\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS htlc_tx_hash TEXT;\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS secret_hash TEXT NOT NULL DEFAULT '';\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS preimage TEXT;\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS timeout_slot BIGINT NOT NULL DEFAULT 0;\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS fee_lovelace BIGINT NOT NULL DEFAULT 0;\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS locked_at TIMESTAMPTZ;\
      \ALTER TABLE route_hops ADD COLUMN IF NOT EXISTS claimed_at TIMESTAMPTZ;"

-- | Insert a new head or update on conflict.
--
-- Bridge status used to be captured here as a per-head flag, but the
-- relay graph now treats every shared-participant pair as a potential
-- bridge automatically — there's no longer a registration-time bridge
-- declaration. The columns @is_bridge@ and @bridge_fee_lovelace@ stay in
-- the schema so older rows aren't disturbed; new inserts always write
-- the defaults.
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
                      , snapshotNumber = lit 0
                      , createdAt = lit now
                      , updatedAt = lit now
                      , lastMessageAt = lit (Just now)
                      , headIsBridge = lit False
                      , headBridgeFeeLovelace = lit Nothing
                      , headRefScriptUtxo = lit Nothing
                      }
                  ]
            , onConflict =
                DoUpdate
                  Upsert
                    { index = (.headId)
                    , predicate = Nothing
                    , set = \new old ->
                        -- Don't clobber a published ref_script_utxo on
                        -- subsequent re-registrations of the same head.
                        new
                          { updatedAt = lit now
                          , lastMessageAt = lit (Just now)
                          , headRefScriptUtxo = old.headRefScriptUtxo
                          }
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

-- | Update snapshot number for a head
updateSnapshotNumber :: Pool -> Text -> Int -> IO ()
updateSnapshotNumber pool hid snapNum = do
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
                  { snapshotNumber = lit (fromIntegral @Int @Int32 snapNum)
                  , updatedAt = lit now
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

-- | Set (or unset, via 'Nothing') the head's published reference-script
-- UTxO. Subsequent lock blueprints for hops in this head will skip the
-- inline-ref-script and reference this UTxO instead.
setHeadRefScriptUtxo :: Pool -> Text -> Maybe Text -> IO ()
setHeadRefScriptUtxo pool hid mUtxo = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = headSchema
            , from = pure ()
            , set = \_ row -> row{headRefScriptUtxo = lit mUtxo, updatedAt = lit now}
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
      , utxoReferenceScriptHash = lit (entry.referenceScript >>= extractReferenceScriptHash)
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

-- | Get UTxOs for an address across all heads, flat list with pagination
getUtxosByAddressFlat :: Pool -> Text -> Int -> Int -> IO [Utxo Identity]
getUtxosByAddressFlat pool addr pageSize page =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.limit (fromIntegral pageSize) $
            Rel8.offset (fromIntegral $ (page - 1) * pageSize) $ do
              u <- Rel8.each utxoSchema
              Rel8.where_ (u.utxoAddress ==. lit addr)
              pure u

-- | Get UTxOs for addresses with snapshot number (for Yoroi-compatible endpoint)
getUtxosByAddressesWithSnapshot :: Pool -> [Text] -> Int -> Int -> IO [(Utxo Identity, Int32)]
getUtxosByAddressesWithSnapshot pool addrs pageSize page =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.limit (fromIntegral pageSize) $
            Rel8.offset (fromIntegral $ (page - 1) * pageSize) $ do
              u <- Rel8.each utxoSchema
              h <- Rel8.each headSchema
              Rel8.where_ (u.utxoHeadId ==. h.headId)
              Rel8.where_ (Rel8.in_ u.utxoAddress (map lit addrs))
              pure (u, h.snapshotNumber)

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

-- | Get stats: total heads, total utxos, heads by status
getStats :: Pool -> IO (Int, Int, Map.Map Text Int)
getStats pool = do
  heads <- getAllHeads pool
  utxos <- runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.each utxoSchema
  let totalHeads = length heads
      totalUtxos = length utxos
      statusCounts = Map.fromListWith (+) [(h.headStatus, 1 :: Int) | h <- heads]
  pure (totalHeads, totalUtxos, statusCounts)

-- | Get explorer stats: unique participants, heads by network, total committed lovelace
getExplorerStats :: Pool -> IO (Int, Map.Map Text Int, Int64)
getExplorerStats pool = getFilteredExplorerStats pool Nothing Nothing

-- | Get explorer stats with optional status and network filters
getFilteredExplorerStats :: Pool -> Maybe Text -> Maybe Text -> IO (Int, Map.Map Text Int, Int64)
getFilteredExplorerStats pool mStatus mNetwork = do
  explorerHeads <- getAllExplorerHeads pool
  let filtered = applyFilters explorerHeads
      headIds = Set.fromList $ map (.explorerHeadId) filtered
  participants <- runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.each headParticipantSchema
  let matchingParticipants = Prelude.filter (\p -> Set.member p.participantHeadId headIds) participants
      uniqueAddresses = Map.keys $ Map.fromList [(p.participantAddress, ()) | p <- matchingParticipants]
      uniqueCount = length uniqueAddresses
      networkCounts = Map.fromListWith (+) [(eh.explorerNetwork, 1 :: Int) | eh <- filtered]
      totalCommitted = Prelude.sum $ map (.participantCommittedLovelace) matchingParticipants
  pure (uniqueCount, networkCounts, totalCommitted)
 where
  applyFilters = Prelude.filter $ \eh ->
    maybe True (\s -> eh.explorerStatus == s) mStatus
      && maybe True (\n -> eh.explorerNetwork == n) mNetwork

-- | Check database connectivity
checkDbConnectivity :: Pool -> IO Bool
checkDbConnectivity pool = do
  result <- Pool.use pool $ Session.sql "SELECT 1"
  pure $ case result of
    Left _ -> False
    Right _ -> True

-- ─── Explorer heads ───

-- | Upsert an explorer head entry
upsertExplorerHead ::
  Pool ->
  Text ->
  Text ->
  Int ->
  Text ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Aeson.Value ->
  Maybe Int ->
  Maybe Aeson.Value ->
  Maybe Text ->
  IO ()
upsertExplorerHead pool hid network networkMagic version status'
  contestationPeriod contestations snapNum contestationDeadline
  point blockNo members seedTxIn = do
    now <- getCurrentTime
    runSession pool $
      Session.statement () $
        Rel8.run_ $
          Rel8.insert
            Insert
              { into = explorerHeadSchema
              , rows =
                  Rel8.values
                    [ ExplorerHead
                        { explorerHeadId = lit hid
                        , explorerNetwork = lit network
                        , explorerNetworkMagic = lit (fromIntegral @Int @Int32 networkMagic)
                        , explorerVersion = lit version
                        , explorerStatus = lit status'
                        , explorerContestationPeriod = lit (fromIntegral @Int @Int32 <$> contestationPeriod)
                        , explorerContestations = lit (fromIntegral @Int @Int32 <$> contestations)
                        , explorerSnapshotNumber = lit (fromIntegral @Int @Int32 <$> snapNum)
                        , explorerContestationDeadline = lit contestationDeadline
                        , explorerPoint = lit point
                        , explorerBlockNo = lit (fromIntegral @Int @Int64 <$> blockNo)
                        , explorerMembers = lit members
                        , explorerSeedTxIn = lit seedTxIn
                        , explorerFirstSeenAt = lit now
                        , explorerLastUpdatedAt = lit now
                        }
                    ]
              , onConflict =
                  DoUpdate
                    Upsert
                      { index = (.explorerHeadId)
                      , predicate = Nothing
                      , set = \new _old ->
                          new{explorerFirstSeenAt = _old.explorerFirstSeenAt, explorerLastUpdatedAt = lit now}
                      , updateWhere = \_ _ -> lit True
                      }
              , returning = NoReturning
              }

-- | Get all explorer heads
getAllExplorerHeads :: Pool -> IO [ExplorerHead Identity]
getAllExplorerHeads pool =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.each explorerHeadSchema

-- | Get all explorer heads with pagination and optional filters
getExplorerHeadsPaginated :: Pool -> Int -> Int -> Maybe Text -> Maybe Text -> IO [ExplorerHead Identity]
getExplorerHeadsPaginated pool pageSize page mStatus mNetwork =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.limit (fromIntegral pageSize) $
            Rel8.offset (fromIntegral $ (page - 1) * pageSize) $ do
              eh <- Rel8.each explorerHeadSchema
              case mStatus of
                Just s -> Rel8.where_ (eh.explorerStatus ==. lit s)
                Nothing -> pure ()
              case mNetwork of
                Just n -> Rel8.where_ (eh.explorerNetwork ==. lit n)
                Nothing -> pure ()
              pure eh

-- | Get a specific explorer head by ID
getExplorerHead :: Pool -> Text -> IO (Maybe (ExplorerHead Identity))
getExplorerHead pool hid =
  runSession pool $ do
    rows <-
      Session.statement () $
        Rel8.run $
          Rel8.select $ do
            eh <- Rel8.each explorerHeadSchema
            Rel8.where_ (eh.explorerHeadId ==. lit hid)
            pure eh
    pure $ case rows of
      [] -> Nothing
      (x : _) -> Just x

-- | Count total explorer heads
countExplorerHeads :: Pool -> IO Int
countExplorerHeads pool = do
  heads <- getAllExplorerHeads pool
  pure $ length heads

-- ─── Head participants ───

-- | Replace all participants for a head (delete + reinsert)
replaceHeadParticipants :: Pool -> Text -> [(Text, Maybe Text, Maybe Text, Int64, Maybe Text)] -> IO ()
replaceHeadParticipants pool hid participants = do
  runSession pool $ do
    -- Delete existing participants for this head
    Session.statement () $
      Rel8.run_ $
        Rel8.delete
          Delete
            { from = headParticipantSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.participantHeadId ==. lit hid
            , returning = NoReturning
            }
    -- Insert new participants
    case participants of
      [] -> pure ()
      _ ->
        Session.statement () $
          Rel8.run_ $
            Rel8.insert
              Insert
                { into = headParticipantSchema
                , rows = Rel8.values $ map toRow participants
                , onConflict = DoNothing
                , returning = NoReturning
                }
 where
  toRow (addr, vkey, onChainId, lovelace, txRef) =
    HeadParticipant
      { participantHeadId = lit hid
      , participantAddress = lit addr
      , participantVkey = lit vkey
      , participantOnChainId = lit onChainId
      , participantCommittedLovelace = lit lovelace
      , participantCommittedTxRef = lit txRef
      }

-- | Get all participants
getAllParticipants :: Pool -> IO [HeadParticipant Identity]
getAllParticipants pool =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $
          Rel8.each headParticipantSchema

-- | Get heads for a participant address
getHeadsByParticipantAddress :: Pool -> Text -> IO [HeadParticipant Identity]
getHeadsByParticipantAddress pool addr =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $ do
          p <- Rel8.each headParticipantSchema
          Rel8.where_ (p.participantAddress ==. lit addr)
          pure p

-- | Get participants for a head
getParticipantsForHead :: Pool -> Text -> IO [HeadParticipant Identity]
getParticipantsForHead pool hid =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $ do
          p <- Rel8.each headParticipantSchema
          Rel8.where_ (p.participantHeadId ==. lit hid)
          pure p

-- | Get heads that have a specific reference script hash in their UTxOs
getHeadsWithScript :: Pool -> Text -> IO [Text]
getHeadsWithScript pool scriptHash =
  runSession pool $ do
    rows <-
      Session.statement () $
        Rel8.run $
          Rel8.select $ do
            u <- Rel8.each utxoSchema
            Rel8.where_ (u.utxoReferenceScriptHash ==. lit (Just scriptHash))
            pure u.utxoHeadId
    pure $ Map.keys $ Map.fromList [(h, ()) | h <- rows]

-- ─── Invoices ───

-- | Insert a new invoice
insertInvoice :: Pool -> Text -> Text -> Text -> Int64 -> Maybe Text -> Text -> UTCTime -> IO ()
insertInvoice pool iid receiverOnChainId payHash amount memo status' expiresAt = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.insert
          Insert
            { into = invoiceSchema
            , rows =
                Rel8.values
                  [ Invoice
                      { invoiceId = lit iid
                      , invoiceReceiverOnChainId = lit receiverOnChainId
                      , invoicePaymentHash = lit payHash
                      , invoiceAmountLovelace = lit amount
                      , invoiceMemo = lit memo
                      , invoiceStatus = lit status'
                      , invoiceExpiresAt = lit expiresAt
                      , invoiceCreatedAt = lit now
                      }
                  ]
            , onConflict = DoNothing
            , returning = NoReturning
            }

-- | Get an invoice by ID
getInvoice :: Pool -> Text -> IO (Maybe (Invoice Identity))
getInvoice pool iid =
  runSession pool $ do
    rows <-
      Session.statement () $
        Rel8.run $
          Rel8.select $ do
            inv <- Rel8.each invoiceSchema
            Rel8.where_ (inv.invoiceId ==. lit iid)
            pure inv
    pure $ case rows of
      [] -> Nothing
      (x : _) -> Just x

-- | Update invoice status
updateInvoiceStatus :: Pool -> Text -> Text -> IO ()
updateInvoiceStatus pool iid newStatus =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = invoiceSchema
            , from = pure ()
            , set = \_ row -> row{invoiceStatus = lit newStatus}
            , updateWhere = \_ row -> row.invoiceId ==. lit iid
            , returning = NoReturning
            }

-- ─── Payment routes ───

-- | Insert a payment route
insertPaymentRoute :: Pool -> Text -> Text -> Text -> Text -> Int64 -> Text -> Aeson.Value -> Int64 -> Text -> IO ()
insertPaymentRoute pool rid invoiceId senderAddr receiverAddr amount status' path totalFee network = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.insert
          Insert
            { into = paymentRouteSchema
            , rows =
                Rel8.values
                  [ PaymentRoute
                      { routeId = lit rid
                      , routeInvoiceId = lit invoiceId
                      , routeSenderAddress = lit senderAddr
                      , routeReceiverAddress = lit receiverAddr
                      , routeAmountLovelace = lit amount
                      , routeStatus = lit status'
                      , routePath = lit path
                      , routeTotalFee = lit totalFee
                      , routeNetwork = lit network
                      , routeCreatedAt = lit now
                      , routeUpdatedAt = lit now
                      }
                  ]
            , onConflict = DoNothing
            , returning = NoReturning
            }

-- | Get a payment route by ID
getPaymentRoute :: Pool -> Text -> IO (Maybe (PaymentRoute Identity))
getPaymentRoute pool rid =
  runSession pool $ do
    rows <-
      Session.statement () $
        Rel8.run $
          Rel8.select $ do
            r <- Rel8.each paymentRouteSchema
            Rel8.where_ (r.routeId ==. lit rid)
            pure r
    pure $ case rows of
      [] -> Nothing
      (x : _) -> Just x

-- | Update payment route status
updateRouteStatus :: Pool -> Text -> Text -> IO ()
updateRouteStatus pool rid newStatus = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = paymentRouteSchema
            , from = pure ()
            , set = \_ row -> row{routeStatus = lit newStatus, routeUpdatedAt = lit now}
            , updateWhere = \_ row -> row.routeId ==. lit rid
            , returning = NoReturning
            }

-- ─── Route hops ───

-- | Insert route hops
insertRouteHops :: Pool -> [(Text, Text, Int, Text, Text, Text, Text, Text, Text, Int64, Int64)] -> IO ()
insertRouteHops pool hops =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.insert
          Insert
            { into = routeHopSchema
            , rows = Rel8.values $ map toRow hops
            , onConflict = DoNothing
            , returning = NoReturning
            }
 where
  toRow (hid, rid, idx, headId, bridgeAddr, senderAddr, receiverAddr, status', secretHash, timeoutSlot, fee) =
    RouteHop
      { hopId = lit hid
      , hopRouteId = lit rid
      , hopIndex = lit (fromIntegral @Int @Int32 idx)
      , hopHeadId = lit headId
      , hopBridgeAddress = lit bridgeAddr
      , hopSenderAddress = lit senderAddr
      , hopReceiverAddress = lit receiverAddr
      , hopHtlcStatus = lit status'
      , hopHtlcTxHash = lit Nothing
      , hopSecretHash = lit secretHash
      , hopPreimage = lit Nothing
      , hopTimeoutSlot = lit timeoutSlot
      , hopFeeLovelace = lit fee
      , hopLockedAt = lit Nothing
      , hopClaimedAt = lit Nothing
      }

-- | Get hops for a route
getRouteHops :: Pool -> Text -> IO [RouteHop Identity]
getRouteHops pool rid =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $ do
          hop <- Rel8.each routeHopSchema
          Rel8.where_ (hop.hopRouteId ==. lit rid)
          pure hop

-- | Update hop HTLC status
updateHopStatus :: Pool -> Text -> Text -> Maybe Text -> IO ()
updateHopStatus pool hopId newStatus mTxHash = do
  now <- getCurrentTime
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = routeHopSchema
            , from = pure ()
            , set = \_ row ->
                row
                  { hopHtlcStatus = lit newStatus
                  , hopHtlcTxHash = case mTxHash of
                      Just tx -> lit (Just tx)
                      Nothing -> row.hopHtlcTxHash
                  , hopLockedAt = case newStatus of
                      "locked" -> lit (Just now)
                      _ -> row.hopLockedAt
                  , hopClaimedAt = case newStatus of
                      "claimed" -> lit (Just now)
                      _ -> row.hopClaimedAt
                  }
            , updateWhere = \_ row -> row.hopId ==. lit hopId
            , returning = NoReturning
            }

-- | Get all active (pending or locked) hops for a given head
getActiveHopsByHead :: Pool -> Text -> IO [RouteHop Identity]
getActiveHopsByHead pool headId =
  runSession pool $
    Session.statement () $
      Rel8.run $
        Rel8.select $ do
          hop <- Rel8.each routeHopSchema
          Rel8.where_ (hop.hopHeadId ==. lit headId)
          Rel8.where_ (hop.hopHtlcStatus ==. lit "pending" ||. hop.hopHtlcStatus ==. lit "locked")
          pure hop

-- | Mark a hop as locked with the HTLC transaction hash
updateHopLocked :: Pool -> Text -> Text -> UTCTime -> IO ()
updateHopLocked pool hopId txHash lockedTime =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = routeHopSchema
            , from = pure ()
            , set = \_ row ->
                row
                  { hopHtlcStatus = lit "locked"
                  , hopHtlcTxHash = lit (Just txHash)
                  , hopLockedAt = lit (Just lockedTime)
                  }
            , updateWhere = \_ row -> row.hopId ==. lit hopId
            , returning = NoReturning
            }

-- | Mark a hop as claimed
updateHopClaimed :: Pool -> Text -> UTCTime -> IO ()
updateHopClaimed pool hopId claimedTime =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = routeHopSchema
            , from = pure ()
            , set = \_ row ->
                row
                  { hopHtlcStatus = lit "claimed"
                  , hopClaimedAt = lit (Just claimedTime)
                  }
            , updateWhere = \_ row -> row.hopId ==. lit hopId
            , returning = NoReturning
            }

-- | Store the revealed preimage on all hops sharing the same secret hash.
-- Once revealed, every hop in the cascade can use it to claim.
setPreimageByHash :: Pool -> Text -> Text -> IO ()
setPreimageByHash pool secretHash preimage =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = routeHopSchema
            , from = pure ()
            , set = \_ row -> row{hopPreimage = lit (Just preimage)}
            , updateWhere = \_ row -> row.hopSecretHash ==. lit secretHash
            , returning = NoReturning
            }

-- | Expire pending invoices past their deadline.
expirePendingInvoices :: Pool -> UTCTime -> IO ()
expirePendingInvoices pool now =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = invoiceSchema
            , from = pure ()
            , set = \_ row -> row{invoiceStatus = lit "expired"}
            , updateWhere = \_ row ->
                row.invoiceStatus ==. lit "pending"
                  &&. row.invoiceExpiresAt Rel8.<. lit now
            , returning = NoReturning
            }

-- | Expire routes whose invoices have expired.
expireStaleRoutes :: Pool -> UTCTime -> IO ()
expireStaleRoutes pool now =
  runSession pool $
    Session.statement () $
      Rel8.run_ $
        Rel8.update
          Update
            { target = paymentRouteSchema
            , from = Rel8.each invoiceSchema
            , set = \_ row -> row{routeStatus = lit "expired", routeUpdatedAt = lit now}
            , updateWhere = \invoice row ->
                row.routeInvoiceId ==. invoice.invoiceId
                  &&. invoice.invoiceStatus ==. lit "expired"
                  &&. (row.routeStatus ==. lit "requested" ||. row.routeStatus ==. lit "in_progress")
            , returning = NoReturning
            }
