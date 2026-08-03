module ApiIntegrationSpec (spec) where

import Api (AppEnv (..), api, server)
import Api.Types
import Db.Schema (AgentRegistration (..))
import Cache (newCache)
import Control.Concurrent.STM
import Data.Aeson (encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Db qualified
import Hydra.Client (HydraEvent, HydraUtxoEntry (..))
import Logging (newLogger)
import Logging qualified
import Metrics (newMetrics)
import Relay.EventBus qualified as EventBus
import Relay.Graph qualified as Graph
import Network.HTTP.Types
import Network.Wai (Application)
import Servant (serve)
import Network.Wai.Test (simpleBody, simpleStatus)
import Test.Hspec
import Test.Hspec.Wai
import TestUtils

spec :: Spec
spec = do
  mainSpec
  secureModeSpec

-- | Tests against the dev-mode app (HYDRA_DIRECT_WS on), matching the
-- behavior of local/testnet workflows.
mainSpec :: Spec
mainSpec = with makeTestApp $ describe "API (integration)" $ do
  describe "GET /" $ do
    it "returns root response with version" $ do
      get "/" `shouldRespondWith` 200

  describe "GET /api/v1/health" $ do
    it "returns ok status with DB connectivity" $ do
      get "/api/v1/health" `shouldRespondWith` 200

  describe "GET /api/v1/heads" $ do
    it "returns list (possibly empty)" $ do
      get "/api/v1/heads" `shouldRespondWith` 200

    it "supports pagination query params" $ do
      get "/api/v1/heads?count=10&page=1" `shouldRespondWith` 200

  describe "GET /api/v1/heads/:headId" $ do
    it "returns 404 for non-existent head" $ do
      get "/api/v1/heads/non-existent" `shouldRespondWith` 404

    it "returns htlcEnabled=false and refScriptUtxo=null when no ref script is set" $ do
      liftIO $ withTestPool $ \pool ->
        Db.upsertHead pool "head-htlc-test" "localhost" 4001 "Open" Nothing
      resp <- get "/api/v1/heads/head-htlc-test"
      liftIO $ case Aeson.decode @Aeson.Value (simpleBody resp) of
        Nothing -> expectationFailure "Could not parse response"
        Just (Aeson.Object o) -> do
          KM.lookup "htlcEnabled" o `shouldBe` Just (Aeson.Bool False)
          KM.lookup "refScriptUtxo" o `shouldBe` Just Aeson.Null
        Just other -> expectationFailure $ "Expected object, got: " <> show other

  describe "GET /api/v1/relay/graph" $ do
    it "returns participant-less open heads as unconnected nodes" $ do
      liftIO $ withTestPool $ \pool -> do
        Db.upsertExplorerHead pool "head-lonely" "Preprod" 1 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "head-closed" "Preprod" 1 "2.2.0" "Closed" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "head-other-net" "Preview" 2 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      resp <- get "/api/v1/relay/graph?network=Preprod"
      liftIO $ case Aeson.decode @Aeson.Value (simpleBody resp) of
        Just (Aeson.Object o) -> do
          case KM.lookup "nodes" o of
            Just (Aeson.Array ns) -> length ns `shouldBe` 1
            _ -> expectationFailure "nodes missing"
          case KM.lookup "edges" o of
            Just (Aeson.Array es) -> length es `shouldBe` 0
            _ -> expectationFailure "edges missing"
        _ -> expectationFailure "Could not parse graph response"

    it "still links heads sharing a participant" $ do
      liftIO $ withTestPool $ \pool -> do
        Db.upsertExplorerHead pool "head-a" "Preprod" 1 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "head-b" "Preprod" 1 "2.2.0" "Open" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.replaceHeadParticipants pool "head-a" [("bridge-pkh", Nothing, Just "bridge-pkh", 0, Nothing)]
        Db.replaceHeadParticipants pool "head-b" [("bridge-pkh", Nothing, Just "bridge-pkh", 0, Nothing)]
      resp <- get "/api/v1/relay/graph?network=Preprod"
      liftIO $ case Aeson.decode @Aeson.Value (simpleBody resp) of
        Just (Aeson.Object o) -> do
          case KM.lookup "nodes" o of
            Just (Aeson.Array ns) -> length ns `shouldBe` 2
            _ -> expectationFailure "nodes missing"
          case KM.lookup "edges" o of
            Just (Aeson.Array es) -> length es `shouldBe` 2
            _ -> expectationFailure "edges missing"
        _ -> expectationFailure "Could not parse graph response"

  describe "agent push endpoints" $ do
    it "register → push pparams for own head; other heads rejected" $ do
      reg <-
        request
          methodPost
          "/api/v1/agent/register"
          [("Content-Type", "application/json")]
          (encode $ Aeson.object [("headId", "head-q"), ("binaryHash", "bin-1"), ("wsUrl", "ws://127.0.0.1:4001")])
      (secret :: T.Text) <- liftIO $ case Aeson.decode @Aeson.Value (simpleBody reg) of
        Just (Aeson.Object o)
          | Just (Aeson.String s) <- KM.lookup "secretKey" o -> pure s
        _ -> fail "could not parse agent registration response"
      let agentHdrs =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> encodeUtf8 secret)
            , ("X-Agent-Binary-Hash", "bin-1")
            ]

      -- protocol parameters: accepted for own head, rejected for others
      request methodPut "/api/v1/agent/heads/head-q/protocol-parameters" agentHdrs (encode $ Aeson.object [("maxTxSize", Aeson.Number 123)])
        `shouldRespondWith` 200
      request methodPut "/api/v1/agent/heads/head-other/protocol-parameters" agentHdrs "{}"
        `shouldRespondWith` 403
      storedPP <- liftIO $ do
        pool <- rawTestPool
        Db.getHeadProtocolParams pool "head-q"
      liftIO $ storedPP `shouldBe` Just (Aeson.object [("maxTxSize", Aeson.Number 123)])

    -- The agent is one-way: there is no command channel for the
    -- registry to push work back to a node. The old poll endpoint
    -- must be gone entirely. (Unmatched POSTs fall through to the
    -- static-site catch-all, which only serves GET — hence 405.)
    it "has no command poll endpoint" $ do
      request methodPost "/api/v1/agent/commands/poll" [("Content-Type", "application/json")] ""
        `shouldRespondWith` 405

  describe "GET /api/v1/heads/:headId/addresses" $ do
    it "returns 404 for non-existent head" $ do
      get "/api/v1/heads/non-existent/addresses" `shouldRespondWith` 404

  describe "GET /api/v1/heads/:headId/addresses/:address/balance" $ do
    it "returns 400 for invalid address" $ do
      get "/api/v1/heads/some-head/addresses/invalid!!!/balance" `shouldRespondWith` 400

    it "returns 404 for non-existent head with valid address" $ do
      get "/api/v1/heads/non-existent/addresses/addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp/balance"
        `shouldRespondWith` 404

  describe "GET /api/v1/heads/:headId/addresses/:address/utxos" $ do
    it "returns 400 for invalid address" $ do
      get "/api/v1/heads/some-head/addresses/invalid!!!/utxos" `shouldRespondWith` 400

  describe "GET /addresses/:address/utxos" $ do
    it "returns 400 for invalid address" $ do
      get "/addresses/invalid!!!/utxos" `shouldRespondWith` 400

    it "returns empty for valid address with no UTxOs" $ do
      get "/addresses/addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp/utxos"
        `shouldRespondWith` 200

  describe "DELETE /api/v1/admin/heads/:headId" $ do
    it "returns 404 for non-existent head" $ do
      delete "/api/v1/admin/heads/non-existent" `shouldRespondWith` 404

  describe "GET /api/v1/metrics" $ do
    it "returns Prometheus-format metrics" $ do
      get "/api/v1/metrics" `shouldRespondWith` 200

  describe "POST /api/v1/heads/register" $ do
    it "returns 400 for unreachable host" $ do
      let body = encode $ RegisterHead "unreachable-host.invalid" 9999 Nothing
      request methodPost "/api/v1/heads/register" [("Content-Type", "application/json")] body
        `shouldRespondWith` 400

  describe "GET /api/v1/explorer/heads" $ do
    it "returns list (possibly empty)" $ do
      get "/api/v1/explorer/heads" `shouldRespondWith` 200

    it "supports pagination and filter query params" $ do
      get "/api/v1/explorer/heads?count=10&page=1&status=Open&network=Mainnet" `shouldRespondWith` 200

  describe "GET /api/v1/explorer/heads/:headId" $ do
    it "returns 404 for non-existent explorer head" $ do
      get "/api/v1/explorer/heads/non-existent" `shouldRespondWith` 404

  describe "GET /api/v1/stats" $ do
    it "returns stats including explorerHeadCount" $ do
      get "/api/v1/stats" `shouldRespondWith` 200

  describe "POST /api/v1/agent/register" $ do
    it "returns 200 with agentId and secretKey (dev mode: empty allowed list accepts any hash)" $ do
      let body = encode $ Aeson.object
            [ "headId"    Aeson..= ("head-xyz" :: String)
            , "binaryHash" Aeson..= ("sha256:abcdef" :: String)
            , "wsUrl"     Aeson..= ("ws://127.0.0.1:19001" :: String)
            ]
      resp <- request methodPost "/api/v1/agent/register" [("Content-Type", "application/json")] body
      liftIO $ case Aeson.decode @Aeson.Value (simpleBody resp) of
        Nothing -> expectationFailure "Could not parse response"
        Just (Aeson.Object o) -> do
          KM.member "agentId" o `shouldBe` True
          KM.member "secretKey" o `shouldBe` True
        Just other -> expectationFailure $ "Expected object, got: " <> show other

    it "does NOT create a head row immediately — head only appears after an Open Greetings is pushed" $ do
      let regBody = encode $ Aeson.object
            [ "headId"    Aeson..= ("agent-lazy-head" :: String)
            , "binaryHash" Aeson..= ("sha256:deadbeef" :: String)
            , "wsUrl"     Aeson..= ("ws://127.0.0.1:19003" :: String)
            ]
      regResp <- request methodPost "/api/v1/agent/register" [("Content-Type", "application/json")] regBody
      -- Extract secretKey from registration response
      secretKey <- liftIO $ case Aeson.decode @Aeson.Value (simpleBody regResp) of
        Just (Aeson.Object o) | Just (Aeson.String sk) <- KM.lookup "secretKey" o -> pure sk
        _ -> fail "could not parse secretKey from register response"
      -- Head must NOT exist yet
      get "/api/v1/heads/agent-lazy-head" `shouldRespondWith` 404
      -- Push an Open Greetings event
      let greetings = Aeson.object
            [ "tag"        Aeson..= ("Greetings" :: String)
            , "headStatus" Aeson..= ("Open" :: String)
            , "hydraHeadId" Aeson..= ("agent-lazy-head" :: String)
            , "currentSlot" Aeson..= (0 :: Int)
            , "snapshotUtxo" Aeson..= Aeson.object []
            , "env" Aeson..= Aeson.object
                ["participants" Aeson..= ([] :: [String])]
            ]
          evtBody = encode $ Aeson.object ["event" Aeson..= greetings]
          authHdr = ("Authorization", "Bearer " <> encodeUtf8 secretKey)
          hashHdr = ("X-Agent-Binary-Hash", "sha256:deadbeef")
      _ <- request methodPost "/api/v1/agent/events"
            [("Content-Type", "application/json"), authHdr, hashHdr] evtBody
      -- Head must now exist as Open
      resp <- get "/api/v1/heads/agent-lazy-head"
      liftIO $ case Aeson.decode @Aeson.Value (simpleBody resp) of
        Nothing -> expectationFailure "could not parse GET /api/v1/heads/agent-lazy-head response"
        Just (Aeson.Object o) -> do
          KM.lookup "headId" o `shouldBe` Just (Aeson.String "agent-lazy-head")
          KM.lookup "host"   o `shouldBe` Just (Aeson.String "127.0.0.1")
          KM.lookup "port"   o `shouldBe` Just (Aeson.Number 19003)
          KM.lookup "status" o `shouldBe` Just (Aeson.String "Open")
        Just other -> expectationFailure $ "expected object, got: " <> show other

  describe "POST /api/v1/agent/events" $ do
    it "returns 401 when Authorization header is missing" $ do
      let body = encode $ Aeson.object ["event" Aeson..= Aeson.object []]
      request methodPost "/api/v1/agent/events" [("Content-Type", "application/json")] body
        `shouldRespondWith` 401

    -- The binary hash is telemetry, not policy: the header is optional,
    -- and a changed hash (agent upgraded its binary) is recorded on the
    -- registration row rather than rejected.
    it "accepts events without a binary-hash header (auth is the secret alone)" $ do
      reg <-
        request
          methodPost
          "/api/v1/agent/register"
          [("Content-Type", "application/json")]
          (encode $ Aeson.object [("headId", "head-nohash"), ("binaryHash", "sha256:v1"), ("wsUrl", "ws://127.0.0.1:4001")])
      (secret :: T.Text) <- liftIO $ case Aeson.decode @Aeson.Value (simpleBody reg) of
        Just (Aeson.Object o)
          | Just (Aeson.String s) <- KM.lookup "secretKey" o -> pure s
        _ -> fail "could not parse agent registration response"
      let body = encode $ Aeson.object ["event" Aeson..= Aeson.object [("tag", "Greetings"), ("headStatus", "Idle")]]
      request methodPost "/api/v1/agent/events"
        [("Content-Type", "application/json"), ("Authorization", "Bearer " <> encodeUtf8 secret)] body
        `shouldRespondWith` 200

    it "records a changed binary hash instead of rejecting (agent upgrade)" $ do
      reg <-
        request
          methodPost
          "/api/v1/agent/register"
          [("Content-Type", "application/json")]
          (encode $ Aeson.object [("headId", "head-upgrade"), ("binaryHash", "sha256:old"), ("wsUrl", "ws://127.0.0.1:4001")])
      (secret, agentId') <- liftIO $ case Aeson.decode @Aeson.Value (simpleBody reg) of
        Just (Aeson.Object o)
          | Just (Aeson.String s) <- KM.lookup "secretKey" o
          , Just (Aeson.String a) <- KM.lookup "agentId" o -> pure (s, a)
        _ -> fail "could not parse agent registration response"
      let body = encode $ Aeson.object ["event" Aeson..= Aeson.object [("tag", "Greetings"), ("headStatus", "Idle")]]
      request methodPost "/api/v1/agent/events"
        [ ("Content-Type", "application/json")
        , ("Authorization", "Bearer " <> encodeUtf8 secret)
        , ("X-Agent-Binary-Hash", "sha256:new")
        ]
        body
        `shouldRespondWith` 200
      stored <- liftIO $ do
        pool <- rawTestPool
        Db.getAgentRegistration pool agentId'
      liftIO $ fmap (.agentBinaryHash) stored `shouldBe` Just "sha256:new"

    it "returns 401 for an unknown agent secret" $ do
      let body = encode $ Aeson.object ["event" Aeson..= Aeson.object []]
      request methodPost "/api/v1/agent/events"
        [("Content-Type", "application/json"), ("Authorization", "Bearer nosuchtoken"), ("X-Agent-Binary-Hash", "sha256:abc")]
        body
        `shouldRespondWith` 401

  describe "POST /api/v1/heads/:headId/claim-ownership" $ do
    it "returns 404 for non-existent head" $ do
      let body = encode $ Aeson.object ["walletAddress" Aeson..= ("addr1qxtest" :: String)]
      request methodPost "/api/v1/heads/nonexistent/claim-ownership" [("Content-Type", "application/json")] body
        `shouldRespondWith` 404

    it "returns 404 when wallet has no UTxO in the head snapshot" $ do
      let claimAddr = "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z" :: T.Text
          body = encode $ Aeson.object ["walletAddress" Aeson..= claimAddr]
      liftIO $ withTestPool $ \pool ->
        Db.upsertHead pool "claim-no-utxo" "localhost" 19001 "Open" Nothing
      request methodPost "/api/v1/heads/claim-no-utxo/claim-ownership" [("Content-Type", "application/json")] body
        `shouldRespondWith` 404

    it "returns 200 and keyHash when wallet has a UTxO in the head snapshot" $ do
      let -- addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z decodes to this pkh
          claimAddr = "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z" :: T.Text
          claimPkh  = "69830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde8" :: T.Text
          utxo = HydraUtxoEntry
            { txHash = "aabbccdd00000000000000000000000000000000000000000000000000000000"
            , outputIndex = 0
            , address = claimAddr
            , lovelace = 5_000_000
            , nativeAssets = mempty
            , datumHash = Nothing
            , inlineDatum = Nothing
            , referenceScript = Nothing
            }
      liftIO $ withTestPool $ \pool -> do
        Db.upsertHead pool "claim-utxo-head" "localhost" 19001 "Open" Nothing
        Db.replaceUtxos pool "claim-utxo-head" [utxo]
      let body = encode $ Aeson.object ["walletAddress" Aeson..= claimAddr]
      resp <- request methodPost "/api/v1/heads/claim-utxo-head/claim-ownership" [("Content-Type", "application/json")] body
      liftIO $ case Aeson.decode @Aeson.Value (simpleBody resp) of
        Nothing -> expectationFailure "Could not parse response"
        Just v -> do
          simpleStatus resp `shouldBe` status200
          case v of
            Aeson.Object o -> KM.lookup "keyHash" o `shouldBe` Just (Aeson.String claimPkh)
            _ -> expectationFailure "Expected JSON object"

  describe "GET /api/v1/relay/invoices" $ do
    it "returns empty list when no invoices exist" $ do
      get "/api/v1/relay/invoices" `shouldRespondWith` 200

    it "returns only pending invoices" $ do
      future <- liftIO $ addUTCTime 3600 <$> getCurrentTime
      liftIO $ withTestPool $ \pool -> do
        Db.insertInvoice pool "all-inv-1" "test-head" "any-pkh" "aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111" 5_000_000 Nothing "pending" future
        Db.insertInvoice pool "all-inv-2" "test-head" "any-pkh" "bbbb2222bbbb2222bbbb2222bbbb2222bbbb2222bbbb2222bbbb2222bbbb2222" 5_000_000 Nothing "paid"    future
      resp <- get "/api/v1/relay/invoices"
      liftIO $ case Aeson.decode @[Aeson.Value] (simpleBody resp) of
        Nothing -> expectationFailure "Could not parse response as JSON array"
        Just invs -> length invs `shouldBe` 1

  describe "GET /api/v1/relay/participants/:pkh/invoices" $ do
    it "returns empty list for unknown receiver" $ do
      get "/api/v1/relay/participants/unknown-pkh/invoices" `shouldRespondWith` 200

    it "returns invoices for a known receiver" $ do
      future <- liftIO $ addUTCTime 3600 <$> getCurrentTime
      liftIO $ withTestPool $ \pool ->
        Db.insertInvoice pool "api-inv-1" "test-head" "api-test-pkh" "aabbccddeeff0011aabbccddeeff0011aabbccddeeff0011aabbccddeeff0011" 5_000_000 Nothing "pending" future
      resp <- get "/api/v1/relay/participants/api-test-pkh/invoices"
      liftIO $ case Aeson.decode @[Aeson.Value] (simpleBody resp) of
        Nothing -> expectationFailure "Could not parse response as JSON array"
        Just invs -> length invs `shouldBe` 1

-- | Create a test Application backed by a test DB
-- | Secure-mode tests: HYDRA_DIRECT_WS off (the production default) —
-- every path that would dial a user's hydra-node must refuse instead.
secureModeSpec :: Spec
secureModeSpec = with (makeTestAppWith False) $ describe "API (secure mode, direct WS disabled)" $ do
  describe "POST /api/v1/heads/register" $ do
    it "refuses direct registration" $ do
      let body = encode $ RegisterHead "some-host" 4001 Nothing
      request methodPost "/api/v1/heads/register" [("Content-Type", "application/json")] body
        `shouldRespondWith` 403

  describe "GET /api/v1/heads/check" $ do
    it "refuses to probe user nodes" $ do
      get "/api/v1/heads/check?host=example.com&port=4001" `shouldRespondWith` 403

  describe "POST /api/v1/heads/:headId/submit" $ do
    -- Users submit signed transactions to their own hydra-node
    -- (POST /transaction on the node API); the registry has no
    -- submission path to any node, in any mode. (Unmatched POSTs
    -- fall through to the static-site catch-all, which only serves
    -- GET — hence 405.)
    it "does not exist — users submit to their own node" $ do
      liftIO $ do
        pool <- rawTestPool
        Db.upsertHead pool "head-no-agent" "127.0.0.1" 4001 "Open" Nothing
      request
        methodPost
        "/api/v1/heads/head-no-agent/submit"
        [("Content-Type", "application/json")]
        (encode $ Aeson.object [("signedCborHex", "84a4beef")])
        `shouldRespondWith` 405

makeTestApp :: IO Application
makeTestApp = makeTestAppWith True

makeTestAppWith :: Bool -> IO Application
makeTestAppWith directWsEnabled = do
  withTestPool $ \pool -> do
    eventQueue <- newTQueueIO @HydraEvent
    metrics <- newMetrics
    addrCache <- newCache 30
    relayGraphVar <- newTVarIO Graph.emptyGraph
    chainSlotVar <- newTVarIO 0
    bus <- EventBus.newEventBus
    let logger = newLogger Logging.Info
        env =
          AppEnv
            { pool = pool
            , eventQueue = eventQueue
            , logger = logger
            , metrics = metrics
            , addressCache = addrCache
            , staticDir = "./website/dist"
            , relayGraph = relayGraphVar
            , latestChainSlot = chainSlotVar
            , relayEventBus = bus
            , htlcScriptHash = Nothing
            , htlcScriptCbor = Nothing
            , cardanoNodeSocket = Nothing
            , cardanoNodeMagic = Nothing
            , directWs = directWsEnabled
            }
    pure $ serve api (server env)
