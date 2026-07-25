module Config where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data AppConfig = AppConfig
  { dbConnStr :: Text
  , httpPort :: Int
  , rateLimitPerMin :: Int
  , healthTimeoutSeconds :: Int
  , staticDir :: FilePath
  , explorerUrl :: Text
  , explorerPollIntervalSeconds :: Int
  , defaultNetwork :: Text
  -- ^ Network label assumed for registered heads the explorer has not
  -- indexed yet; the relay graph only routes between same-network heads.
  , htlcScriptHash :: Maybe Text
  , htlcScriptCbor :: Maybe Text
  , blockfrostProjectId :: Maybe Text
  , blockfrostNetwork :: Text
  , cardanoNodeSocket :: Maybe FilePath
  , cardanoNodeMagic :: Maybe Int
  , agentAllowedHashes :: [Text]
  , l1Sockets :: [(Text, FilePath)]
  -- ^ Per-network local cardano-node sockets for the L1 head scan
  -- (network name → socket path).
  , directWs :: Bool
  -- ^ Allow the registry to dial user hydra-node APIs directly
  -- (registration probes, startup WS reconnects, submit/pparams
  -- fallbacks). Off by default: the hydra-node API is unauthenticated,
  -- so production must rely exclusively on the agent push model.
  -- Enable only for local dev/testnet where nodes run on localhost.
  }
  deriving stock (Show, Eq)

defaultConfig :: AppConfig
defaultConfig =
  AppConfig
    { dbConnStr = "host=/tmp port=5432 dbname=hydra_registry"
    , httpPort = 8080
    , rateLimitPerMin = 100
    , healthTimeoutSeconds = 120
    , staticDir = "./website/dist"
    , explorerUrl = "https://explorer.hydra.family"
    , explorerPollIntervalSeconds = 120
    , defaultNetwork = "Preview"
    , agentAllowedHashes = []
    , htlcScriptHash = Just "81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df"
    , htlcScriptCbor = Just "5903d401010029800aba4aba2aba1aba0aab9faab9eaab9dab9cab9a4888888888c96600264653001300a00198051805800cdc3a4005300a0024888966002600460146ea800e2646644b300100789919912cc004c00c006264b300100180744c96600200300f807c03e01f13259800980c001c01602080a8dd7000a0303015001404c60226ea802a2b300130080018acc004c044dd5005400a01a809201a807100e0acc004c004c038dd50014660026024601e6ea800a46026602860286028602860286028602860280032232330010010032259800800c528456600266e3cdd7180b000801c528c4cc008008c05c005010202891809980a000c8c04cc050c050c050c050c050c050c0500052222259800980318099baa00d8992cc004cc0052410f505245494d414745204641494c4544005980099b8f37286eb8c060c054dd50071bae30183015375400d14a3153301349012b626c616b6532625f32353628707265696d61676529203d3d20646174756d2e68617368203f2046616c73650014a080922b3001330014910c56414c4944204245464f5245005980099191919912cc004c044c064dd50014566002602260326ea8c074c07800e266e20dd6980e980d1baa002001899b89375a603a60346ea80080050174528202e301b001375a600c60306ea8024cc064c068004cc0666002601c602c6ea8c068c06c00698103d87a8000a60103d8798000405097ae0301637546008602c6ea8004c008c054dd5005c528c54cc04d24013676616c69645f6265666f72652874782e76616c69646974795f72616e67652c20646174756d2e74696d656f757429203f2046616c73650014a080922660086eb0c014c054dd50059bae30183019301930193015375400d14a080922941012112cc00400629462a660280042c809a2b30013232323322598009808180c1baa0028acc004c040c060dd5180e180e801c4cdc40009bad301c301937540051337120026eb4c070c064dd5001202c8a50405860340026eb4c014c05cdd50041980c180c8009980c4c004c034c054dd5180c980d000d30103d87a8000a60103d8798000404c97ae0301537546030602a6ea8004c004c050dd500544cc00cdd61802180a1baa00a375c602e6030603060286ea801629410112022454cc0352411e65787065637420536f6d6528646174756d29203d20646174756d5f6f707400164030601c6ea8020dc3a400100a805402a0148098c03c004c03cc040004c02cdd5001c590080c028004c014dd5005c5268a99801a491856616c696461746f722072657475726e65642066616c7365001365640082a6600492011272656465656d65723a2052656465656d6572001601"
    , blockfrostProjectId = Nothing
    , blockfrostNetwork = "preview"
    , cardanoNodeSocket = Nothing
    , cardanoNodeMagic = Nothing
    , l1Sockets = []
    , directWs = False
    }

loadConfig :: IO AppConfig
loadConfig = do
  dbConn <- lookupEnvText "HYDRA_DB_CONN_STR" defaultConfig.dbConnStr
  port <- lookupEnvRead "HYDRA_HTTP_PORT" defaultConfig.httpPort
  rateLimit <- lookupEnvRead "HYDRA_RATE_LIMIT" defaultConfig.rateLimitPerMin
  healthTimeout <- lookupEnvRead "HYDRA_HEALTH_TIMEOUT" defaultConfig.healthTimeoutSeconds
  staticDirPath <- lookupEnvString "HYDRA_STATIC_DIR" defaultConfig.staticDir
  explorerUrlVal <- lookupEnvText "HYDRA_EXPLORER_URL" defaultConfig.explorerUrl
  explorerPoll <- lookupEnvRead "HYDRA_EXPLORER_POLL_INTERVAL" defaultConfig.explorerPollIntervalSeconds
  defaultNet <- lookupEnvText "HYDRA_DEFAULT_NETWORK" defaultConfig.defaultNetwork
  htlcHash <- lookupEnvMaybeWithDefault "HYDRA_HTLC_SCRIPT_HASH" defaultConfig.htlcScriptHash
  htlcCbor <- lookupEnvMaybeWithDefault "HYDRA_HTLC_SCRIPT_CBOR" defaultConfig.htlcScriptCbor
  bfProjectId <- lookupEnvMaybe "BLOCKFROST_PROJECT_ID"
  bfNetwork <- lookupEnvText "BLOCKFROST_NETWORK" defaultConfig.blockfrostNetwork
  nodeSocket <- lookupEnvString' "CARDANO_NODE_SOCKET_PATH"
  nodeMagic <- lookupEnvReadMaybe "CARDANO_NODE_MAGIC"
  agentHashes <- lookupEnvTextList "HYDRA_AGENT_ALLOWED_HASHES"
  l1SocketList <- lookupL1Sockets
  directWsVal <- lookupEnvBool "HYDRA_DIRECT_WS" defaultConfig.directWs
  pure
    AppConfig
      { dbConnStr = dbConn
      , httpPort = port
      , rateLimitPerMin = rateLimit
      , healthTimeoutSeconds = healthTimeout
      , staticDir = staticDirPath
      , explorerUrl = explorerUrlVal
      , explorerPollIntervalSeconds = explorerPoll
      , defaultNetwork = defaultNet
      , htlcScriptHash = htlcHash
      , htlcScriptCbor = htlcCbor
      , blockfrostProjectId = bfProjectId
      , blockfrostNetwork = bfNetwork
      , cardanoNodeSocket = nodeSocket
      , cardanoNodeMagic = nodeMagic
      , agentAllowedHashes = agentHashes
      , l1Sockets = l1SocketList
      , directWs = directWsVal
      }

-- | Boolean env var: "true"/"1" (case-insensitive) enable, anything
-- else falls back to the default.
lookupEnvBool :: String -> Bool -> IO Bool
lookupEnvBool key def = do
  mVal <- lookupEnv key
  pure $ case T.toLower . T.pack <$> mVal of
    Just v -> v == "true" || v == "1"
    Nothing -> def

-- | Collect per-network cardano-node sockets from
-- HYDRA_L1_SOCKET_{PREPROD,PREVIEW,MAINNET}.
lookupL1Sockets :: IO [(Text, FilePath)]
lookupL1Sockets =
  fmap catMaybes . mapM lookupOne $
    [ ("Preprod", "HYDRA_L1_SOCKET_PREPROD")
    , ("Preview", "HYDRA_L1_SOCKET_PREVIEW")
    , ("Mainnet", "HYDRA_L1_SOCKET_MAINNET")
    ]
 where
  lookupOne (net, key) = fmap (net,) <$> lookupEnv key

lookupEnvText :: String -> Text -> IO Text
lookupEnvText key def = do
  mVal <- lookupEnv key
  pure $ maybe def T.pack mVal

lookupEnvRead :: (Read a) => String -> a -> IO a
lookupEnvRead key def = do
  mVal <- lookupEnv key
  pure $ fromMaybe def (mVal >>= readMaybe)

lookupEnvString :: String -> String -> IO String
lookupEnvString key def = do
  mVal <- lookupEnv key
  pure $ fromMaybe def mVal

lookupEnvMaybe :: String -> IO (Maybe Text)
lookupEnvMaybe key = do
  mVal <- lookupEnv key
  pure $ T.pack <$> mVal

lookupEnvMaybeWithDefault :: String -> Maybe Text -> IO (Maybe Text)
lookupEnvMaybeWithDefault key def = do
  mVal <- lookupEnv key
  pure $ case mVal of
    Just v -> Just (T.pack v)
    Nothing -> def

lookupEnvString' :: String -> IO (Maybe FilePath)
lookupEnvString' key = lookupEnv key

lookupEnvReadMaybe :: (Read a) => String -> IO (Maybe a)
lookupEnvReadMaybe key = do
  mVal <- lookupEnv key
  pure $ mVal >>= readMaybe

-- | Read a comma-separated list of Text values from an env var (empty list if unset/empty).
lookupEnvTextList :: String -> IO [Text]
lookupEnvTextList key = do
  mVal <- lookupEnv key
  pure $ case mVal of
    Nothing -> []
    Just "" -> []
    Just v -> T.splitOn "," (T.pack v)
