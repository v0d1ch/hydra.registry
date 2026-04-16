module TestUtils where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Db qualified
import Hasql.Pool (Pool)
import Hasql.Session qualified as Session
import Hydra.Client (HydraUtxoEntry (..))
import System.Environment (lookupEnv)

-- | Create a sample HydraUtxoEntry for testing
sampleUtxoEntry :: HydraUtxoEntry
sampleUtxoEntry =
  HydraUtxoEntry
    { txHash = "abc123def456"
    , outputIndex = 0
    , address = "addr1qxtest"
    , lovelace = 5_000_000
    , nativeAssets = Map.empty
    , datumHash = Nothing
    , inlineDatum = Nothing
    , referenceScript = Nothing
    }

-- | Create a sample UTxO entry with native assets
sampleUtxoEntryWithAssets :: HydraUtxoEntry
sampleUtxoEntryWithAssets =
  sampleUtxoEntry
    { nativeAssets =
        Map.fromList
          [ ( "policy1"
            , Map.fromList [("token1", 100), ("token2", 200)]
            )
          ]
    }

-- | Get a test DB pool and run an action with it.
-- Uses TEST_DB_CONN_STR env var, falls back to a local test database.
withTestPool :: (Pool -> IO a) -> IO a
withTestPool action = do
  mConnStr <- lookupEnv "TEST_DB_CONN_STR"
  let connStr = T.pack $ case mConnStr of
        Just s -> s
        Nothing -> "host=/tmp port=5432 dbname=hydra_registry_test"
  pool <- Db.createPool connStr
  Db.initDb pool
  cleanTestData pool
  action pool

-- | Remove all test data
cleanTestData :: Pool -> IO ()
cleanTestData pool =
  Db.runSession pool $ do
    Session.sql "DELETE FROM utxos"
    Session.sql "DELETE FROM heads"

-- | Build a Greetings JSON message for testing
mkGreetingsJson :: Text -> Text -> [(Key.Key, Value)] -> Value
mkGreetingsJson hid hstatus utxos =
  Object $
    KM.fromList
      [ ("tag", String "Greetings")
      , ("hydraHeadId", String hid)
      , ("headStatus", String hstatus)
      , ("snapshotUtxo", Object (KM.fromList utxos))
      ]

-- | Build a SnapshotConfirmed JSON message for testing
mkSnapshotConfirmedJson :: Text -> [(Key.Key, Value)] -> Value
mkSnapshotConfirmedJson hid utxos =
  Object $
    KM.fromList
      [ ("tag", String "SnapshotConfirmed")
      , ("headId", String hid)
      ,
        ( "snapshot"
        , Object $
            KM.fromList
              [("utxo", Object (KM.fromList utxos))]
        )
      ]

-- | Build a HeadIsClosed JSON message for testing
mkHeadIsClosedJson :: Text -> Value
mkHeadIsClosedJson hid =
  Object $
    KM.fromList
      [ ("tag", String "HeadIsClosed")
      , ("headId", String hid)
      ]

-- | Build a HeadIsFinalized JSON message for testing
mkHeadIsFinalizedJson :: Text -> [(Key.Key, Value)] -> Value
mkHeadIsFinalizedJson hid utxos =
  Object $
    KM.fromList
      [ ("tag", String "HeadIsFinalized")
      , ("headId", String hid)
      , ("utxo", Object (KM.fromList utxos))
      ]

-- | Build a sample TxOut JSON value for UTxO map entries
mkTxOutJson :: Text -> Integer -> Value
mkTxOutJson addr lovelaceAmt =
  Object $
    KM.fromList
      [ ("address", String addr)
      , ("value", Object $ KM.fromList [("lovelace", Number (fromInteger lovelaceAmt))])
      ]

-- | Build a sample TxOut with native assets
mkTxOutWithAssetsJson :: Text -> Integer -> [(Key.Key, [(Key.Key, Integer)])] -> Value
mkTxOutWithAssetsJson addr lovelaceAmt assets =
  Object $
    KM.fromList
      [ ("address", String addr)
      ,
        ( "value"
        , Object $
            KM.fromList $
              ("lovelace", Number (fromInteger lovelaceAmt))
                : [ (k, Object $ KM.fromList [(ak, Number (fromInteger av)) | (ak, av) <- avs])
                  | (k, avs) <- assets
                  ]
        )
      ]
