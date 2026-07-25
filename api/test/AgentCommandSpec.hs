module AgentCommandSpec (spec) where

import Agent.CommandQueue (awaitCommand, newCommandWaiters, resolveCommand)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.Functor.Identity (Identity)
import Data.Time (addUTCTime, getCurrentTime)
import Db qualified
import Db.Schema (AgentCommand (..), AgentRegistration (..))
import Hydra.Submit (SubmitResult (..))
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "Agent command queue" $ do
  describe "CommandWaiters (in-process)" $ do
    it "delivers a resolved result to the waiting caller" $ do
      waiters <- newCommandWaiters
      void . forkIO $ do
        threadDelay 50_000
        void $ resolveCommand waiters "cmd-1" (SubmitValid "abc123")
      result <- awaitCommand waiters "cmd-1" 5
      result `shouldBe` Just (SubmitValid "abc123")

    it "times out when nobody resolves" $ do
      waiters <- newCommandWaiters
      result <- awaitCommand waiters "cmd-never" 1
      result `shouldBe` Nothing

    it "resolving an unknown command reports False" $ do
      waiters <- newCommandWaiters
      ok <- resolveCommand waiters "cmd-unknown" SubmitTimeout
      ok `shouldBe` False

  describe "agent_commands lifecycle (integration)" $ around withTestPool $ do
    it "insert → claim marks delivered and returns payload; second claim is empty" $ \pool -> do
      Db.insertAgentCommand pool "cmd-a" "head-1" "submit_tx" "84a4..."
      claimed <- Db.claimPendingCommands pool "head-1"
      case claimed of
        [c :: AgentCommand Identity] -> do
          c.commandId `shouldBe` "cmd-a"
          c.commandKind `shouldBe` "submit_tx"
          c.commandPayload `shouldBe` "84a4..."
        other -> expectationFailure $ "expected one command, got " <> show (length other)
      again <- Db.claimPendingCommands pool "head-1"
      again `shouldBe` []

    it "claim only returns commands for the requested head" $ \pool -> do
      Db.insertAgentCommand pool "cmd-b" "head-1" "submit_tx" "aa"
      Db.insertAgentCommand pool "cmd-c" "head-2" "submit_tx" "bb"
      claimed <- Db.claimPendingCommands pool "head-2"
      map (.commandId) claimed `shouldBe` ["cmd-c"]

    it "complete stores the result JSON and finishes the command" $ \pool -> do
      Db.insertAgentCommand pool "cmd-d" "head-1" "submit_tx" "cc"
      _ <- Db.claimPendingCommands pool "head-1"
      Db.completeAgentCommand pool "cmd-d" (Aeson.toJSON (SubmitValid "tx99"))
      mCmd <- Db.getAgentCommand pool "cmd-d"
      case mCmd of
        Nothing -> expectationFailure "command disappeared"
        Just c -> do
          c.commandStatus `shouldBe` "done"
          c.commandResult `shouldBe` Just (Aeson.toJSON (SubmitValid "tx99"))

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

  describe "lookupActiveAgentForHead (integration)" $ around withTestPool $ do
    it "finds an agent seen recently, ignores stale ones" $ \pool -> do
      now <- getCurrentTime
      Db.insertAgentRegistration pool "ag-fresh" "head-x" "hash-1" "bin" "127.0.0.1" 4001
      Db.updateAgentLastSeen pool "ag-fresh" now
      Db.insertAgentRegistration pool "ag-stale" "head-y" "hash-2" "bin" "127.0.0.1" 4002
      Db.updateAgentLastSeen pool "ag-stale" (addUTCTime (-3600) now)
      fresh <- Db.lookupActiveAgentForHead pool "head-x" 120
      fmap (.agentId) fresh `shouldBe` Just "ag-fresh"
      stale <- Db.lookupActiveAgentForHead pool "head-y" 120
      stale `shouldBe` Nothing
