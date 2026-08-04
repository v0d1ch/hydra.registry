module AgentPushSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Db qualified
import Hasql.Session qualified as Session
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "Agent push model" $ do
  describe "one-way agent (integration)" $ around withTestPool $ do
    -- The agent conveys information to the registry and nothing else:
    -- there is no command channel back to the node, so the old queue
    -- table must not exist in a fresh schema.
    it "has no agent_commands table - the registry cannot queue node writes" $ \pool -> do
      result <-
        try @SomeException $
          Db.runSession pool $
            Session.sql "SELECT 1 FROM agent_commands LIMIT 1"
      case result of
        Left _ -> pure ()
        Right _ ->
          expectationFailure
            "agent_commands table still exists - the registry can still queue tx submissions"

  describe "head_protocol_params (integration)" $ around withTestPool $ do
    it "stores and retrieves protocol parameters per head" $ \pool -> do
      let pparams = Aeson.object [("maxTxSize", Aeson.Number 16384)]
      Db.setHeadProtocolParams pool "head-pp" pparams
      got <- Db.getHeadProtocolParams pool "head-pp"
      got `shouldBe` Just pparams
      Db.getHeadProtocolParams pool "head-none" >>= (`shouldBe` Nothing)

    it "overwrites on re-push" $ \pool -> do
      Db.setHeadProtocolParams pool "head-pp" (Aeson.object [("v", Aeson.Number 1)])
      Db.setHeadProtocolParams pool "head-pp" (Aeson.object [("v", Aeson.Number 2)])
      got <- Db.getHeadProtocolParams pool "head-pp"
      got `shouldBe` Just (Aeson.object [("v", Aeson.Number 2)])

  describe "push-model heads (integration)" $ around withTestPool $ do
    it "allows two heads reporting the same ws host/port" $ \pool -> do
      -- every push-model agent reports its node as 127.0.0.1:4001; the
      -- old UNIQUE(host,port) constraint made the second head explode
      Db.upsertHead pool "head-same-1" "127.0.0.1" 4001 "Open" Nothing
      Db.upsertHead pool "head-same-2" "127.0.0.1" 4001 "Open" Nothing
      h1 <- Db.getHead pool "head-same-1"
      h2 <- Db.getHead pool "head-same-2"
      h1 `shouldSatisfy` (/= Nothing)
      h2 `shouldSatisfy` (/= Nothing)
