module SidecarSpec (spec) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Map.Strict qualified as Map
import Db qualified
import Explorer.Sidecar (SidecarConfig (..), rebuildRelayGraph)
import Logging (newLogger)
import Logging qualified
import Relay.Graph qualified as Graph
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "Explorer.Sidecar rebuildRelayGraph (integration)" $ around withTestPool $ do
  it "labels registered-only heads with the configured default network" $ \pool -> do
    Db.upsertHead pool "head-reg-preprod" "localhost" 4001 "Open" Nothing
    Db.replaceHeadParticipants pool "head-reg-preprod" [("pkh-a", Nothing, Just "pkh-a", 0, Nothing)]
    graphVar <- newTVarIO Graph.emptyGraph
    let config =
          SidecarConfig
            { explorerUrl = "http://127.0.0.1:1" -- unreachable on purpose; rebuild is poll-independent
            , pollIntervalSeconds = 1
            , relayGraphVar = graphVar
            , defaultNetwork = "Preprod"
            , l1Sockets = []
            }
    rebuildRelayGraph (newLogger Logging.Error) pool config
    g <- readTVarIO graphVar
    case Map.lookup "head-reg-preprod" g.graphNodes of
      Nothing -> expectationFailure "registered head missing from rebuilt graph"
      Just node -> node.nodeNetwork `shouldBe` "Preprod"

  it "keeps the explorer-reported network for explorer-known heads" $ \pool -> do
    Db.upsertHead pool "head-known" "localhost" 4002 "Open" Nothing
    Db.upsertExplorerHead
      pool
      "head-known"
      "Preprod"
      1
      "2.3.0"
      "Open"
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
    graphVar <- newTVarIO Graph.emptyGraph
    let config =
          SidecarConfig
            { explorerUrl = "http://127.0.0.1:1"
            , pollIntervalSeconds = 1
            , relayGraphVar = graphVar
            , defaultNetwork = "Preview"
            , l1Sockets = []
            }
    rebuildRelayGraph (newLogger Logging.Error) pool config
    g <- readTVarIO graphVar
    case Map.lookup "head-known" g.graphNodes of
      Nothing -> expectationFailure "explorer-known head missing from rebuilt graph"
      Just node -> node.nodeNetwork `shouldBe` "Preprod"
