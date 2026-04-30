module RelayGraphSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Relay.Graph
  ( HtlcHop (..)
  , RelayGraph (..)
  , Route (..)
  , RouteHop (..)
  , buildGraph
  , expandRouteToHtlcs
  , findRoutes
  )
import Test.Hspec

spec :: Spec
spec = describe "Relay.Graph" $ do
  describe "buildGraph" $ do
    it "creates an edge between two heads sharing a participant" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview")]
          participants = [("head-A", "addr_ida"), ("head-B", "addr_ida")]
          graph = buildGraph heads participants
      length graph.graphEdges `shouldBe` 2 -- (A→B) + (B→A)

    it "ignores edges across different networks" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preprod")]
          participants = [("head-A", "addr_ida"), ("head-B", "addr_ida")]
          graph = buildGraph heads participants
      graph.graphEdges `shouldBe` []

    it "drops shared-participant groups larger than 10" $ do
      -- A common test key landing in 11 heads should not produce edges
      -- between every pair. The cap keeps the graph workable.
      let mkH i = "h" <> T.pack (show i)
          heads = [(mkH i, "Preview" :: Text) | i <- [0 .. 10 :: Int]]
          participants = [(mkH i, "addr_dust" :: Text) | i <- [0 .. 10]]
          graph = buildGraph heads participants
      graph.graphEdges `shouldBe` []

  describe "findRoutes" $ do
    it "finds a single-edge route between two heads" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview")]
          participants =
            [ ("head-A", "addr_alice")
            , ("head-A", "addr_ida")
            , ("head-B", "addr_ida")
            , ("head-B", "addr_bob")
            ]
          graph = buildGraph heads participants
      case findRoutes graph "addr_alice" "addr_bob" "Preview" 3 of
        [r] -> do
          r.routeSrcHead `shouldBe` "head-A"
          length r.routeHops `shouldBe` 1
          (head r.routeHops).hopHeadId `shouldBe` "head-B"
        rs -> expectationFailure ("expected 1 route, got " <> show (length rs))

    it "returns empty for disconnected heads" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview")]
          participants =
            [ ("head-A", "addr_alice")
            , ("head-B", "addr_bob")
            ]
          graph = buildGraph heads participants
      findRoutes graph "addr_alice" "addr_bob" "Preview" 3 `shouldBe` []

    it "finds a 2-edge route via intermediate head" $ do
      let heads = [("A", "Preview"), ("B", "Preview"), ("C", "Preview")]
          participants =
            [ ("A", "alice")
            , ("A", "ida")
            , ("B", "ida")
            , ("B", "jim")
            , ("C", "jim")
            , ("C", "bob")
            ]
          graph = buildGraph heads participants
      case findRoutes graph "alice" "bob" "Preview" 3 of
        (r : _) -> do
          r.routeSrcHead `shouldBe` "A"
          length r.routeHops `shouldBe` 2
          map (.hopHeadId) r.routeHops `shouldBe` ["B", "C"]
          map (.hopBridgeAddress) r.routeHops `shouldBe` ["ida", "jim"]
        [] -> expectationFailure "expected at least one route"

  describe "expandRouteToHtlcs" $ do
    it "E=0 (sender and receiver in same head) → one HTLC" $ do
      let route = Route "head-A" [] 0
          htlcs = expandRouteToHtlcs "alice" "bob" route
      htlcs
        `shouldBe` [ HtlcHop
                       { htlcHopHeadId = "head-A"
                       , htlcHopSender = "alice"
                       , htlcHopReceiver = "bob"
                       , htlcHopFee = 0
                       }
                   ]

    it "E=1 → two HTLCs, one per head, bridge in the middle" $ do
      let route =
            Route
              "head-A"
              [RouteHop "head-B" "ida" 0]
              0
          htlcs = expandRouteToHtlcs "alice" "bob" route
      htlcs
        `shouldBe` [ HtlcHop "head-A" "alice" "ida" 0
                   , HtlcHop "head-B" "ida" "bob" 0
                   ]

    it "E=2 → three HTLCs, two distinct bridges, sender/receiver chained" $ do
      let route =
            Route
              "head-A"
              [ RouteHop "head-B" "ida" 100
              , RouteHop "head-C" "jim" 200
              ]
              300
          htlcs = expandRouteToHtlcs "alice" "bob" route
      htlcs
        `shouldBe` [ HtlcHop "head-A" "alice" "ida" 100
                   , HtlcHop "head-B" "ida" "jim" 200
                   , HtlcHop "head-C" "jim" "bob" 0
                   ]
      -- The last HTLC's fee is 0 because there's no edge after it; the
      -- per-edge fee belongs to the bridge spending out of that edge.
      length htlcs `shouldBe` 3

    it "preserves the head sequence even when sender pkh appears in many heads" $ do
      -- Mirrors the "popular sender pkh" case: alice's hex appears in
      -- multiple heads but the dijkstra picked one specific src head;
      -- expansion must use that one and ignore the rest.
      let route =
            Route
              "head-AliceMain"
              [RouteHop "head-Bob" "ida" 0]
              0
          htlcs = expandRouteToHtlcs "alice" "bob" route
      map (.htlcHopHeadId) htlcs `shouldBe` ["head-AliceMain", "head-Bob"]
