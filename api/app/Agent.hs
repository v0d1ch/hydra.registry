module Main (main) where

import Agent.BinaryHash (getBinaryHash)
import Agent.CommandPoller (commandLoop, pushProtocolParams)
import Agent.EventPusher (AgentState (..), loadOrRegister, pushEvent)
import Agent.ReadOnly (ReadOnlyConn, withReadOnlyConn, receive)
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, catch, throwIO)
import Data.Aeson (Value, decode)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  wsUrl       <- requireEnv "HYDRA_NODE_WS_URL"
  registryUrl <- T.pack <$> requireEnv "HYDRA_REGISTRY_URL"
  stateFile   <- requireEnv' "HYDRA_AGENT_STATE_FILE" ".hydra-agent-state.json"

  binaryHash <- getBinaryHash
  TIO.hPutStrLn stderr $ "Binary hash: " <> binaryHash

  mgr <- newManager tlsManagerSettings

  let (wsHost, wsPort, wsPath) = parseWsUrl wsUrl
  TIO.hPutStrLn stderr $ "Connecting to hydra-node at " <> T.pack wsUrl

  withReadOnlyConn wsHost wsPort wsPath $ \conn -> do
    TIO.hPutStrLn stderr "Connected. Waiting for Greetings..."
    firstMsg <- receive conn
    headId' <- case decode @AgentGreeting firstMsg of
      Just g -> pure g.hydraHeadId
      Nothing -> fail "First message is not Greetings — cannot determine head ID"

    TIO.hPutStrLn stderr $ "Head ID: " <> headId'
    st <- loadOrRegister mgr stateFile registryUrl headId' (T.pack wsUrl) binaryHash
    TIO.hPutStrLn stderr $ "Agent ID: " <> st.agentId

    -- Push the local node's protocol parameters so the registry never
    -- has to fetch them from this node.
    pushProtocolParams mgr registryUrl st binaryHash headId' wsHost wsPort

    case decode @Value firstMsg of
      Just v -> pushEvent mgr registryUrl st binaryHash v
      Nothing -> pure ()

    -- Event pushing and command polling run side by side; if either
    -- dies (e.g. the node WS drops) the agent exits and systemd (or the
    -- operator) restarts it.
    race_
      (loop conn mgr registryUrl st binaryHash)
      (commandLoop mgr registryUrl st binaryHash wsHost wsPort)

loop :: ReadOnlyConn -> Manager -> Text -> AgentState -> Text -> IO ()
loop conn mgr registryUrl st binaryHash = do
  msg <- receive conn `catch` \(e :: SomeException) -> do
    TIO.hPutStrLn stderr $ "WebSocket error: " <> T.pack (show e)
    throwIO e
  case decode @Value msg of
    Just v -> do
      pushEvent mgr registryUrl st binaryHash v
      loop conn mgr registryUrl st binaryHash
    Nothing ->
      loop conn mgr registryUrl st binaryHash

requireEnv :: String -> IO String
requireEnv key = do
  mVal <- lookupEnv key
  case mVal of
    Just v -> pure v
    Nothing -> fail $ "Required env var not set: " <> key

requireEnv' :: String -> String -> IO String
requireEnv' key def = fmap (maybe def id) (lookupEnv key)

-- | Parse @ws://host:port/path@ (TLS-stripped) into (host, port, path).
parseWsUrl :: String -> (String, Int, String)
parseWsUrl url =
  let stripped = case T.stripPrefix "wss://" (T.pack url) of
        Just rest -> T.unpack rest
        Nothing -> case T.stripPrefix "ws://" (T.pack url) of
          Just rest -> T.unpack rest
          Nothing   -> url
      (hostPort, path') = case break (== '/') stripped of
        (hp, []) -> (hp, "/")
        (hp, p)  -> (hp, p)
      (host', portStr) = case break (== ':') hostPort of
        (h, ':' : p) -> (h, p)
        (h, _)       -> (h, "4001")
      port' = case reads portStr of
        [(n, "")] -> n
        _         -> 4001
  in (host', port', path')

-- | Minimal shape for extracting headId from Greetings.
newtype AgentGreeting = AgentGreeting {hydraHeadId :: Text}

instance Aeson.FromJSON AgentGreeting where
  parseJSON = Aeson.withObject "AgentGreeting" $ \o ->
    AgentGreeting <$> o Aeson..: "hydraHeadId"
