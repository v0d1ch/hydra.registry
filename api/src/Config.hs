module Config where

import Data.Maybe (fromMaybe)
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
  , htlcScriptHash :: Maybe Text
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
    , htlcScriptHash = Nothing
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
  htlcHash <- lookupEnvMaybe "HYDRA_HTLC_SCRIPT_HASH"
  pure
    AppConfig
      { dbConnStr = dbConn
      , httpPort = port
      , rateLimitPerMin = rateLimit
      , healthTimeoutSeconds = healthTimeout
      , staticDir = staticDirPath
      , explorerUrl = explorerUrlVal
      , explorerPollIntervalSeconds = explorerPoll
      , htlcScriptHash = htlcHash
      }

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
