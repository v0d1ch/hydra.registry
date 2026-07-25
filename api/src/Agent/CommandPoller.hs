-- | The agent side of the command queue: poll the registry for queued
-- commands, execute them against the agent's /local/ hydra-node, and
-- report results back. Together with the event pusher this makes every
-- connection outbound from the operator's machine — the registry never
-- needs network access to the hydra-node API.
module Agent.CommandPoller
  ( commandLoop
  , pushProtocolParams
  ) where

import Agent.EventPusher (AgentState (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (Value, decode, encode)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Hydra.Submit (SubmitResult (..), submitToHead)
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
import Network.HTTP.Types.Status (statusCode)
import System.IO (stderr)

-- | Seconds between polls. Cheap (one small authenticated POST), and the
-- registry's submit handler waits ~30s, so a couple of seconds is plenty.
pollIntervalSeconds :: Int
pollIntervalSeconds = 2

-- | Minimal mirror of the registry's @AgentCommandInfo@.
data PolledCommand = PolledCommand
  { commandId :: Text
  , kind :: Text
  , payload :: Text
  }

instance Aeson.FromJSON PolledCommand where
  parseJSON = Aeson.withObject "PolledCommand" $ \o ->
    PolledCommand
      <$> o Aeson..: "commandId"
      <*> o Aeson..: "kind"
      <*> o Aeson..: "payload"

agentRequest :: AgentState -> Text -> Request -> Request
agentRequest st binaryHash req =
  req
    { requestHeaders =
        [ ("Content-Type", "application/json")
        , ("Authorization", "Bearer " <> TE.encodeUtf8 st.secretKey)
        , ("X-Agent-Binary-Hash", TE.encodeUtf8 binaryHash)
        ]
    }

-- | Fetch the local node's protocol parameters over its HTTP API (same
-- port as the WS) and push them to the registry so server-side tx
-- building reads them from the DB instead of dialing this node.
pushProtocolParams :: Manager -> Text -> AgentState -> Text -> Text -> String -> Int -> IO ()
pushProtocolParams mgr registryUrl st binaryHash headId' nodeHost nodePort = do
  result <- try @SomeException $ do
    ppReq <- parseRequest $ "http://" <> nodeHost <> ":" <> show nodePort <> "/protocol-parameters"
    ppResp <- httpLbs ppReq mgr
    case decode @Value (responseBody ppResp) of
      Nothing -> fail "could not parse protocol-parameters from local node"
      Just pparams -> do
        req <- parseRequest $ T.unpack registryUrl <> "/api/v1/agent/heads/" <> T.unpack headId' <> "/protocol-parameters"
        let req' = (agentRequest st binaryHash req){method = "PUT", requestBody = RequestBodyLBS (encode pparams)}
        resp <- httpLbs req' mgr
        pure (statusCode (responseStatus resp))
  case result of
    Right code | code < 300 -> TIO.hPutStrLn stderr "Pushed protocol parameters to registry"
    Right code -> TIO.hPutStrLn stderr $ "Protocol parameters push rejected: HTTP " <> T.pack (show code)
    Left e -> TIO.hPutStrLn stderr $ "Protocol parameters push failed: " <> T.pack (show e)

-- | Poll for commands forever. Failures are logged and the loop keeps
-- going — a broken registry connection must not kill the agent.
commandLoop :: Manager -> Text -> AgentState -> Text -> String -> Int -> IO ()
commandLoop mgr registryUrl st binaryHash nodeHost nodePort = forever $ do
  result <- try @SomeException pollOnce
  case result of
    Left e -> TIO.hPutStrLn stderr $ "Command poll failed: " <> T.pack (show e)
    Right () -> pure ()
  threadDelay (pollIntervalSeconds * 1_000_000)
 where
  pollOnce = do
    req <- parseRequest $ T.unpack registryUrl <> "/api/v1/agent/commands/poll"
    let req' = (agentRequest st binaryHash req){method = "POST"}
    resp <- httpLbs req' mgr
    case decode @[PolledCommand] (responseBody resp) of
      Nothing -> pure ()
      Just cmds -> mapM_ execute cmds

  execute cmd = do
    TIO.hPutStrLn stderr $ "Executing command " <> cmd.commandId <> " (" <> cmd.kind <> ")"
    outcome <- case cmd.kind of
      "submit_tx" -> submitToHead (T.pack nodeHost) nodePort cmd.payload
      other -> pure $ SubmitInvalid{reason = "unsupported command kind: " <> other}
    report cmd.commandId outcome

  report cmdId outcome = do
    req <- parseRequest $ T.unpack registryUrl <> "/api/v1/agent/commands/" <> T.unpack cmdId <> "/result"
    let req' = (agentRequest st binaryHash req){method = "POST", requestBody = RequestBodyLBS (encode outcome)}
    resp <- httpLbs req' mgr
    let code = statusCode (responseStatus resp)
    if code < 300
      then TIO.hPutStrLn stderr $ "Reported result for " <> cmdId
      else TIO.hPutStrLn stderr $ "Result report rejected for " <> cmdId <> ": HTTP " <> T.pack (show code)
