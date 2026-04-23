module RelayGraphSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Relay.Graph (RelayGraph (..), Route (..), RouteHop (..), buildGraph, findRoutes)
import Test.Hspec

spec :: Spec
spec = describe "Relay.Graph" $ do
  describe "buildGraph" $ do
    it "creates edges between heads sharing a bridge participant" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview")]
          participants = [("head-A", "addr_ida"), ("head-B", "addr_ida")]
          bridges = Set.fromList ["head-A"]
          fees = Map.fromList [("head-A", 500000)]
          htlc = Set.fromList ["head-A", "head-B"]
          graph = buildGraph heads participants bridges fees htlc
      length graph.graphEdges `shouldSatisfy` (> 0)

    it "does not create edges when HTLC script is missing" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview")]
          participants = [("head-A", "addr_ida"), ("head-B", "addr_ida")]
          bridges = Set.fromList ["head-A"]
          fees = Map.fromList [("head-A", 500000)]
          htlc = Set.fromList ["head-A"] -- head-B missing HTLC
          graph = buildGraph heads participants bridges fees htlc
      length graph.graphEdges `shouldBe` 0

    it "does not create edges across different networks" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preprod")]
          participants = [("head-A", "addr_ida"), ("head-B", "addr_ida")]
          bridges = Set.fromList ["head-A"]
          fees = Map.fromList [("head-A", 500000)]
          htlc = Set.fromList ["head-A", "head-B"]
          graph = buildGraph heads participants bridges fees htlc
      length graph.graphEdges `shouldBe` 0

  describe "findRoutes" $ do
    it "finds a single-hop route between two heads" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview")]
          participants =
            [ ("head-A", "addr_alice")
            , ("head-A", "addr_ida")
            , ("head-B", "addr_ida")
            , ("head-B", "addr_bob")
            ]
          bridges = Set.fromList ["head-A", "head-B"]
          fees = Map.fromList [("head-A", 500000), ("head-B", 500000)]
          htlc = Set.fromList ["head-A", "head-B"]
          graph = buildGraph heads participants bridges fees htlc
          routes = findRoutes graph "addr_alice" "addr_bob" "Preview" 3
      length routes `shouldSatisfy` (> 0)

    it "returns empty for disconnected heads" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview")]
          participants =
            [ ("head-A", "addr_alice")
            , ("head-B", "addr_bob")
            ]
          bridges = Set.fromList ["head-A", "head-B"]
          fees = Map.empty
          htlc = Set.fromList ["head-A", "head-B"]
          graph = buildGraph heads participants bridges fees htlc
          routes = findRoutes graph "addr_alice" "addr_bob" "Preview" 3
      length routes `shouldBe` 0

    it "finds multi-hop route through intermediate heads" $ do
      let heads = [("head-A", "Preview"), ("head-B", "Preview"), ("head-C", "Preview")]
          participants =
            [ ("head-A", "addr_alice")
            , ("head-A", "addr_ida")
            , ("head-B", "addr_ida")
            , ("head-B", "addr_jim")
            , ("head-C", "addr_jim")
            , ("head-C", "addr_bob")
            ]
          bridges = Set.fromList ["head-A", "head-B", "head-C"]
          fees = Map.fromList [("head-A", 500000), ("head-B", 300000), ("head-C", 200000)]
          htlc = Set.fromList ["head-A", "head-B", "head-C"]
          graph = buildGraph heads participants bridges fees htlc
          routes = findRoutes graph "addr_alice" "addr_bob" "Preview" 3
      length routes `shouldSatisfy` (> 0)
      let r = Prelude.head routes
      length r.routeHops `shouldBe` 2

  describe "hop sender/receiver calculation" $ do
    it "correctly assigns sender/receiver for a 2-hop route" $ do
      let hops =
            [ RouteHop{hopHeadId = "head-A", hopBridgeAddress = "addr_ida", hopFee = 500000}
            , RouteHop{hopHeadId = "head-B", hopBridgeAddress = "addr_ida", hopFee = 500000}
            ]
          senderAddr = "addr_alice"
          receiverAddr = "addr_bob"
          numHops = length hops
          hopSR idx h =
            let sender =
                  if idx == 0
                    then senderAddr
                    else (hops !! (idx - 1)).hopBridgeAddress
                receiver =
                  if idx == numHops - 1
                    then receiverAddr
                    else h.hopBridgeAddress
             in (sender, receiver)
      hopSR 0 (hops !! 0) `shouldBe` ("addr_alice", "addr_ida")
      hopSR 1 (hops !! 1) `shouldBe` ("addr_ida", "addr_bob")

    it "correctly assigns sender/receiver for a 3-hop route" $ do
      let hops =
            [ RouteHop{hopHeadId = "head-A", hopBridgeAddress = "addr_ida", hopFee = 500000}
            , RouteHop{hopHeadId = "head-B", hopBridgeAddress = "addr_jim", hopFee = 300000}
            , RouteHop{hopHeadId = "head-C", hopBridgeAddress = "addr_jim", hopFee = 200000}
            ]
          senderAddr = "addr_alice"
          receiverAddr = "addr_bob"
          numHops = length hops
          hopSR idx h =
            let sender =
                  if idx == 0
                    then senderAddr
                    else (hops !! (idx - 1)).hopBridgeAddress
                receiver =
                  if idx == numHops - 1
                    then receiverAddr
                    else h.hopBridgeAddress
             in (sender, receiver)
      hopSR 0 (hops !! 0) `shouldBe` ("addr_alice", "addr_ida")
      hopSR 1 (hops !! 1) `shouldBe` ("addr_ida", "addr_jim")
      hopSR 2 (hops !! 2) `shouldBe` ("addr_jim", "addr_bob")
