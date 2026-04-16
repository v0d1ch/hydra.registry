module Hydra.Client where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Logging (Logger, logError, logInfo, logWarn)
import Network.WebSockets qualified as WS

-- | Parsed UTxO entry from Hydra
data HydraUtxoEntry = HydraUtxoEntry
  { txHash :: Text
  , outputIndex :: Int
  , address :: Text
  , lovelace :: Integer
  , nativeAssets :: Map.Map Text (Map.Map Text Integer)
  , datumHash :: Maybe Text
  , inlineDatum :: Maybe Value
  , referenceScript :: Maybe Value
  }
  deriving stock (Eq, Show)

-- | Events emitted by the Hydra client
data HydraEvent
  = HeadGreetings
      { greeterHeadId :: Text
      , greeterHeadStatus :: Text
      , greeterUtxos :: [HydraUtxoEntry]
      }
  | HeadSnapshotConfirmed
      { snapHeadId :: Text
      , snapNumber :: Int
      , snapUtxos :: [HydraUtxoEntry]
      }
  | HeadClosed {closedHeadId :: Text}
  | HeadFinalized
      { finalizedHeadId :: Text
      , finalizedUtxos :: [HydraUtxoEntry]
      }
  | ConnectionLost {lostHeadId :: Text}
  deriving stock (Eq, Show)

-- | Parse a Hydra WebSocket message into an event
parseHydraMessage :: Value -> Maybe HydraEvent
parseHydraMessage = \case
  Object obj -> do
    String tag <- KM.lookup "tag" obj
    case tag of
      "Greetings" -> do
        let hid = case KM.lookup "hydraHeadId" obj of
              Just (String s) -> s
              _ -> ""
        let hstatus = case KM.lookup "headStatus" obj of
              Just (String s) -> s
              _ -> "Unknown"
        let utxoEntries = case KM.lookup "snapshotUtxo" obj of
              Just v -> parseUtxoMap v
              _ -> []
        Just $ HeadGreetings hid hstatus utxoEntries
      "SnapshotConfirmed" -> do
        hid <- case KM.lookup "headId" obj of
          Just (String s) -> Just s
          _ -> Nothing
        let (snapNum, utxoEntries) = case KM.lookup "snapshot" obj of
              Just (Object snap) ->
                let num = case KM.lookup "number" snap of
                      Just (Number n) -> round n
                      _ -> 0
                    utxos = case KM.lookup "utxo" snap of
                      Just v -> parseUtxoMap v
                      _ -> []
                 in (num, utxos)
              _ -> (0, [])
        Just $ HeadSnapshotConfirmed hid snapNum utxoEntries
      "HeadIsClosed" -> do
        hid <- case KM.lookup "headId" obj of
          Just (String s) -> Just s
          _ -> Nothing
        Just $ HeadClosed hid
      "HeadIsFinalized" -> do
        hid <- case KM.lookup "headId" obj of
          Just (String s) -> Just s
          _ -> Nothing
        let utxoEntries = case KM.lookup "utxo" obj of
              Just v -> parseUtxoMap v
              _ -> []
        Just $ HeadFinalized hid utxoEntries
      _ -> Nothing
  _ -> Nothing

-- | Parse a Hydra UTxO map (keys are "txhash#index", values are TxOut objects)
parseUtxoMap :: Value -> [HydraUtxoEntry]
parseUtxoMap (Object utxoObj) =
  concatMap parseEntry (KM.toList utxoObj)
 where
  parseEntry (key, Object txOut) =
    let keyText = Key.toText key
     in case T.splitOn "#" keyText of
          [txh, idx] | Just i <- readMaybe' (T.unpack idx) ->
            [ HydraUtxoEntry
                { txHash = txh
                , outputIndex = i
                , address = getTextField txOut "address"
                , lovelace = getLovelace txOut
                , nativeAssets = getNativeAssets txOut
                , datumHash = getOptionalTextField txOut "datumhash"
                , inlineDatum = KM.lookup "inlineDatum" txOut >>= nonNull
                , referenceScript = KM.lookup "referenceScript" txOut >>= nonNull
                }
            ]
          _ -> []
  parseEntry _ = []
parseUtxoMap _ = []

getTextField :: KM.KeyMap Value -> Key.Key -> Text
getTextField obj k = case KM.lookup k obj of
  Just (String s) -> s
  _ -> ""

getOptionalTextField :: KM.KeyMap Value -> Key.Key -> Maybe Text
getOptionalTextField obj k = case KM.lookup k obj of
  Just (String s) -> Just s
  _ -> Nothing

nonNull :: Value -> Maybe Value
nonNull Null = Nothing
nonNull v = Just v

getLovelace :: KM.KeyMap Value -> Integer
getLovelace obj = case KM.lookup "value" obj of
  Just (Object valObj) -> case KM.lookup "lovelace" valObj of
    Just (Number n) -> round n
    _ -> 0
  _ -> 0

getNativeAssets :: KM.KeyMap Value -> Map.Map Text (Map.Map Text Integer)
getNativeAssets obj = case KM.lookup "value" obj of
  Just (Object valObj) ->
    Map.fromList
      [ (Key.toText k, parseAssetMap v)
      | (k, v) <- KM.toList valObj
      , Key.toText k /= "lovelace"
      ]
  _ -> Map.empty
 where
  parseAssetMap (Object assets) =
    Map.fromList
      [ (Key.toText k, round n)
      | (k, Number n) <- KM.toList assets
      ]
  parseAssetMap _ = Map.empty

readMaybe' :: (Read a) => String -> Maybe a
readMaybe' s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- | Validate a Hydra node by connecting and parsing the Greetings message
validateHydraNode :: Logger -> Text -> Int -> IO (Either Text HydraEvent)
validateHydraNode logger hostAddr portNum = do
  result <- try @SomeException $ do
    WS.runClient (T.unpack hostAddr) portNum "/" $ \conn -> do
      msg <- WS.receiveData conn
      case decode msg of
        Nothing -> pure $ Left "Failed to parse message from Hydra node"
        Just val -> case parseHydraMessage val of
          Just evt@HeadGreetings{greeterHeadId}
            | T.null greeterHeadId ->
                pure $ Left "Head is in Idle state (no head ID)"
            | otherwise ->
                pure $ Right evt
          _ -> pure $ Left "First message was not a Greetings message"
  case result of
    Left err -> do
      logError logger "Connection to Hydra node failed" [("error", toJSON (show err))]
      pure $ Left $ "Connection failed: " <> T.pack (show err)
    Right r -> pure r

-- | Connect to a Hydra node and continuously listen for events.
-- Each head connection runs in its own thread with independent error handling.
connectToHead :: Logger -> Text -> Text -> Int -> TQueue HydraEvent -> IO ()
connectToHead logger headId hostAddr portNum eventQueue = void $ async $ reconnectLoop 1
 where
  maxDelay :: Int
  maxDelay = 300

  reconnectLoop :: Int -> IO ()
  reconnectLoop delay = do
    listenResult <- try @SomeException $ do
      WS.runClient (T.unpack hostAddr) portNum "/" $ \conn -> do
        logInfo logger "Connected to Hydra head" [("headId", toJSON headId), ("host", toJSON hostAddr)]
        forever $ do
          msg <- WS.receiveData conn
          case decode msg >>= parseHydraMessage of
            Just evt -> atomically $ writeTQueue eventQueue evt
            Nothing -> pure ()
    case listenResult of
      Left err -> do
        logWarn
          logger
          "WebSocket connection lost, reconnecting..."
          [ ("headId", toJSON headId)
          , ("host", toJSON hostAddr)
          , ("delay_seconds", toJSON delay)
          , ("error", toJSON (show err))
          ]
        atomically $ writeTQueue eventQueue (ConnectionLost headId)
        threadDelay (delay * 1_000_000)
        reconnectLoop (min (delay * 2) maxDelay)
      Right () ->
        reconnectLoop 1
