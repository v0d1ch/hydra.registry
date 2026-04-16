module ApiIntegrationSpec (spec) where

import Api (AppEnv (..), api, server)
import Api.Types
import Cache (newCache)
import Control.Concurrent.STM
import Data.Aeson (decode, encode)
import Db qualified
import Hydra.Client (HydraEvent)
import Logging (newLogger)
import Logging qualified
import Metrics (newMetrics)
import Network.HTTP.Types
import Network.Wai (Application)
import Servant (serve)
import Test.Hspec
import Test.Hspec.Wai
import TestUtils

spec :: Spec
spec = with makeTestApp $ describe "API (integration)" $ do
  describe "GET /" $ do
    it "returns root response with version" $ do
      get "/" `shouldRespondWith` 200

  describe "GET /health" $ do
    it "returns ok status with DB connectivity" $ do
      get "/health" `shouldRespondWith` 200

  describe "GET /heads" $ do
    it "returns list (possibly empty)" $ do
      get "/heads" `shouldRespondWith` 200

    it "supports pagination query params" $ do
      get "/heads?count=10&page=1" `shouldRespondWith` 200

  describe "GET /heads/:headId" $ do
    it "returns 404 for non-existent head" $ do
      get "/heads/non-existent" `shouldRespondWith` 404

  describe "GET /heads/:headId/addresses" $ do
    it "returns 404 for non-existent head" $ do
      get "/heads/non-existent/addresses" `shouldRespondWith` 404

  describe "GET /heads/:headId/addresses/:address/balance" $ do
    it "returns 400 for invalid address" $ do
      get "/heads/some-head/addresses/invalid!!!/balance" `shouldRespondWith` 400

    it "returns 404 for non-existent head with valid address" $ do
      get "/heads/non-existent/addresses/addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp/balance"
        `shouldRespondWith` 404

  describe "GET /heads/:headId/addresses/:address/utxos" $ do
    it "returns 400 for invalid address" $ do
      get "/heads/some-head/addresses/invalid!!!/utxos" `shouldRespondWith` 400

  describe "GET /addresses/:address/utxos" $ do
    it "returns 400 for invalid address" $ do
      get "/addresses/invalid!!!/utxos" `shouldRespondWith` 400

    it "returns empty for valid address with no UTxOs" $ do
      get "/addresses/addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp/utxos"
        `shouldRespondWith` 200

  describe "DELETE /admin/heads/:headId" $ do
    it "returns 404 for non-existent head" $ do
      delete "/admin/heads/non-existent" `shouldRespondWith` 404

  describe "GET /metrics" $ do
    it "returns Prometheus-format metrics" $ do
      get "/metrics" `shouldRespondWith` 200

  describe "POST /heads/register" $ do
    it "returns 400 for unreachable host" $ do
      let body = encode $ RegisterHead "unreachable-host.invalid" 9999
      request methodPost "/heads/register" [("Content-Type", "application/json")] body
        `shouldRespondWith` 400

-- | Create a test Application backed by a test DB
makeTestApp :: IO Application
makeTestApp = do
  withTestPool $ \pool -> do
    eventQueue <- newTQueueIO @HydraEvent
    metrics <- newMetrics
    addrCache <- newCache 30
    let logger = newLogger Logging.Info
        env =
          AppEnv
            { pool = pool
            , eventQueue = eventQueue
            , logger = logger
            , metrics = metrics
            , addressCache = addrCache
            }
    pure $ serve api (server env)
