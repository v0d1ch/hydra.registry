module ConfigSpec (spec) where

import Config
import System.Environment (setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = describe "Config" $ do
  describe "defaultConfig" $ do
    it "has expected default values" $ do
      defaultConfig.httpPort `shouldBe` 8080
      defaultConfig.rateLimitPerMin `shouldBe` 100
      defaultConfig.dbConnStr `shouldBe` "host=/tmp port=5432 dbname=hydra_registry"
      defaultConfig.healthTimeoutSeconds `shouldBe` 120

    it "defaults the fallback network to Preview" $ do
      defaultConfig.defaultNetwork `shouldBe` "Preview"

  describe "HYDRA_DEFAULT_NETWORK" $ do
    it "overrides the fallback network for registered-only heads" $ do
      setEnv "HYDRA_DEFAULT_NETWORK" "Preprod"
      config <- loadConfig
      unsetEnv "HYDRA_DEFAULT_NETWORK"
      config.defaultNetwork `shouldBe` "Preprod"

  describe "HYDRA_L1_SOCKET_*" $ do
    it "collects per-network cardano-node sockets" $ do
      setEnv "HYDRA_L1_SOCKET_PREPROD" "/tmp/preprod.socket"
      setEnv "HYDRA_L1_SOCKET_MAINNET" "/tmp/mainnet.socket"
      config <- loadConfig
      unsetEnv "HYDRA_L1_SOCKET_PREPROD"
      unsetEnv "HYDRA_L1_SOCKET_MAINNET"
      lookup "Preprod" config.l1Sockets `shouldBe` Just "/tmp/preprod.socket"
      lookup "Mainnet" config.l1Sockets `shouldBe` Just "/tmp/mainnet.socket"
      lookup "Preview" config.l1Sockets `shouldBe` Nothing

    it "defaults to no sockets" $ do
      defaultConfig.l1Sockets `shouldBe` []

  describe "HYDRA_DIRECT_WS" $ do
    it "defaults to disabled (registry never dials user nodes)" $ do
      defaultConfig.directWs `shouldBe` False

    it "can be enabled for dev/testnet workflows" $ do
      setEnv "HYDRA_DIRECT_WS" "true"
      config <- loadConfig
      unsetEnv "HYDRA_DIRECT_WS"
      config.directWs `shouldBe` True

    it "treats other values as disabled" $ do
      setEnv "HYDRA_DIRECT_WS" "banana"
      config <- loadConfig
      unsetEnv "HYDRA_DIRECT_WS"
      config.directWs `shouldBe` False

  describe "loadConfig" $ do
    it "loads config (uses defaults when env vars not set)" $ do
      -- This test uses whatever env vars are currently set
      -- In CI, we'd set specific env vars to test overrides
      config <- loadConfig
      config.httpPort `shouldSatisfy` (> 0)
      config.rateLimitPerMin `shouldSatisfy` (> 0)
