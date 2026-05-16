module Agent.EventPusher
  ( AgentState (..)
  , loadOrRegister
  , pushEvent
  ) where

import Data.Aeson (Value, encode, decode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody (..)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (status200, status201)
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
loadOrRegister :: Manager -> FilePath -> Text -> Text -> Text -> IO AgentState
loadOrRegister mgr stateFile registryUrl headId' binaryHash = do
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
      st <- register mgr registryUrl headId' binaryHash
      BSL.writeFile stateFile (encode st)
      pure st

register :: Manager -> Text -> Text -> Text -> IO AgentState
register mgr registryUrl headId' binaryHash = do
  let url = T.unpack registryUrl <> "/api/v1/agent/register"
      body = encode $ Aeson.object ["headId" Aeson..= headId', "binaryHash" Aeson..= binaryHash]
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
