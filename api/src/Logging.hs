module Logging where

import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text (Text)
import Data.Time (getCurrentTime)

data LogLevel = Debug | Info | Warn | Error
  deriving stock (Show, Eq, Ord)

instance ToJSON LogLevel where
  toJSON = \case
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Error -> "error"

data Logger = Logger
  { minLevel :: LogLevel
  }

newLogger :: LogLevel -> Logger
newLogger = Logger

logMsg :: Logger -> LogLevel -> Text -> [(Text, Value)] -> IO ()
logMsg logger lvl msg extra
  | lvl >= logger.minLevel = do
      now <- getCurrentTime
      let entry =
            object $
              [ "timestamp" .= show now
              , "level" .= lvl
              , "message" .= msg
              ]
                <> map (\(k, v) -> Key.fromText k .= v) extra
      BSL.putStrLn $ encode entry
  | otherwise = pure ()

logInfo :: Logger -> Text -> [(Text, Value)] -> IO ()
logInfo l = logMsg l Info

logWarn :: Logger -> Text -> [(Text, Value)] -> IO ()
logWarn l = logMsg l Warn

logError :: Logger -> Text -> [(Text, Value)] -> IO ()
logError l = logMsg l Error

logDebug :: Logger -> Text -> [(Text, Value)] -> IO ()
logDebug l = logMsg l Debug
