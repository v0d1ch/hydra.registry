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
  }
  deriving stock (Show, Eq)

defaultConfig :: AppConfig
defaultConfig =
  AppConfig
    { dbConnStr = "host=/tmp port=5432 dbname=hydra_registry"
    , httpPort = 8080
    , rateLimitPerMin = 100
    , healthTimeoutSeconds = 120
    }

loadConfig :: IO AppConfig
loadConfig = do
  dbConn <- lookupEnvText "HYDRA_DB_CONN_STR" defaultConfig.dbConnStr
  port <- lookupEnvRead "HYDRA_HTTP_PORT" defaultConfig.httpPort
  rateLimit <- lookupEnvRead "HYDRA_RATE_LIMIT" defaultConfig.rateLimitPerMin
  healthTimeout <- lookupEnvRead "HYDRA_HEALTH_TIMEOUT" defaultConfig.healthTimeoutSeconds
  pure
    AppConfig
      { dbConnStr = dbConn
      , httpPort = port
      , rateLimitPerMin = rateLimit
      , healthTimeoutSeconds = healthTimeout
      }

lookupEnvText :: String -> Text -> IO Text
lookupEnvText key def = do
  mVal <- lookupEnv key
  pure $ maybe def T.pack mVal

lookupEnvRead :: (Read a) => String -> a -> IO a
lookupEnvRead key def = do
  mVal <- lookupEnv key
  pure $ fromMaybe def (mVal >>= readMaybe)
