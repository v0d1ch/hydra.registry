module ConfigSpec (spec) where

import Config
import Test.Hspec

spec :: Spec
spec = describe "Config" $ do
  describe "defaultConfig" $ do
    it "has expected default values" $ do
      defaultConfig.httpPort `shouldBe` 8080
      defaultConfig.rateLimitPerMin `shouldBe` 100
      defaultConfig.dbConnStr `shouldBe` "host=/tmp port=5432 dbname=hydra_registry"
      defaultConfig.healthTimeoutSeconds `shouldBe` 120

  describe "loadConfig" $ do
    it "loads config (uses defaults when env vars not set)" $ do
      -- This test uses whatever env vars are currently set
      -- In CI, we'd set specific env vars to test overrides
      config <- loadConfig
      config.httpPort `shouldSatisfy` (> 0)
      config.rateLimitPerMin `shouldSatisfy` (> 0)
