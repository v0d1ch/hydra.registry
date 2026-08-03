module Agent.EventPusher
  ( AgentState (..)
  , loadOrRegister
  , pushEvent
  , pushProtocolParams
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, encode, decode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
  ( Manager
  , RequestBody (..)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (status200, status201, statusCode)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Persisted agent credentials.
data AgentState = AgentState
  { agentId :: Text
  , secretKey :: Text
  }
  deriving stock (Show)

instance Aeson.FromJSON AgentState where
  parseJSON = Aeson.withObject "AgentState" $ \o ->
    AgentState <$> o Aeson..: "agentId" <*> o Aeson..: "secretKey"

instance Aeson.ToJSON AgentState where
  toJSON s = Aeson.object ["agentId" Aeson..= s.agentId, "secretKey" Aeson..= s.secretKey]

-- | Load existing agent state from file, or register with the registry and persist it.
loadOrRegister :: Manager -> FilePath -> Text -> Text -> Text -> Text -> IO AgentState
loadOrRegister mgr stateFile registryUrl headId' wsUrl binaryHash = do
  exists <- doesFileExist stateFile
  if exists
    then do
      bytes <- BSL.readFile stateFile
      case decode @AgentState bytes of
        Just st -> pure st
        Nothing -> do
          hPutStrLn stderr "State file is corrupt. Delete it and restart."
          exitFailure
    else do
      st <- register mgr registryUrl headId' wsUrl binaryHash
      BSL.writeFile stateFile (encode st)
      pure st

register :: Manager -> Text -> Text -> Text -> Text -> IO AgentState
register mgr registryUrl headId' wsUrl binaryHash = do
  let url = T.unpack registryUrl <> "/api/v1/agent/register"
      body = encode $ Aeson.object ["headId" Aeson..= headId', "binaryHash" Aeson..= binaryHash, "wsUrl" Aeson..= wsUrl]
  req <- parseRequest url
  let req' = req
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS body
        }
  resp <- httpLbs req' mgr
  let status = responseStatus resp
  if status == status200 || status == status201
    then case decode @AgentState (responseBody resp) of
      Just st -> pure st
      Nothing -> do
        hPutStrLn stderr "Could not parse registration response from registry"
        exitFailure
    else do
      hPutStrLn stderr $ "Registration failed: " <> show status
      exitFailure

-- | Push a raw event JSON to the registry's agent events endpoint.
pushEvent :: Manager -> Text -> AgentState -> Text -> Value -> IO ()
pushEvent mgr registryUrl st binaryHash event = do
  let url = T.unpack registryUrl <> "/api/v1/agent/events"
      body = encode $ Aeson.object ["event" Aeson..= event]
  req <- parseRequest url
  let req' = req
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> T.encodeUtf8 st.secretKey)
            , ("X-Agent-Binary-Hash", T.encodeUtf8 binaryHash)
            ]
        , requestBody = RequestBodyLBS body
        }
  resp <- httpLbs req' mgr
  let status = responseStatus resp
  if status == status200 || status == status201
    then pure ()
    else hPutStrLn stderr $ "Event push failed: " <> show status

-- | Fetch the local node's protocol parameters over its HTTP API (same
-- port as the WS) and push them to the registry so server-side tx
-- building reads them from the DB instead of dialing this node. Like
-- everything else the agent does, this only conveys information: a
-- read from the local node, a write to the registry.
pushProtocolParams :: Manager -> Text -> AgentState -> Text -> Text -> String -> Int -> IO ()
pushProtocolParams mgr registryUrl st binaryHash headId' nodeHost nodePort = do
  result <- try @SomeException $ do
    ppReq <- parseRequest $ "http://" <> nodeHost <> ":" <> show nodePort <> "/protocol-parameters"
    ppResp <- httpLbs ppReq mgr
    case decode @Value (responseBody ppResp) of
      Nothing -> fail "could not parse protocol-parameters from local node"
      Just pparams -> do
        req <- parseRequest $ T.unpack registryUrl <> "/api/v1/agent/heads/" <> T.unpack headId' <> "/protocol-parameters"
        let req' =
              req
                { method = "PUT"
                , requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> T.encodeUtf8 st.secretKey)
                    , ("X-Agent-Binary-Hash", T.encodeUtf8 binaryHash)
                    ]
                , requestBody = RequestBodyLBS (encode pparams)
                }
        resp <- httpLbs req' mgr
        pure (statusCode (responseStatus resp))
  case result of
    Right code | code < 300 -> hPutStrLn stderr "Pushed protocol parameters to registry"
    Right code -> hPutStrLn stderr $ "Protocol parameters push rejected: HTTP " <> show code
    Left e -> hPutStrLn stderr $ "Protocol parameters push failed: " <> show e
