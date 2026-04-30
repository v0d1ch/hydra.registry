module Relay.Graph where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Hydra.Htlc qualified as Htlc

-- | A node in the relay graph
data GraphNode = GraphNode
  { nodeHeadId :: Text
  , nodeNetwork :: Text
  , nodeHasHtlc :: Bool
  }
  deriving stock (Eq, Show)

-- | A directed edge between two heads via a bridge operator
data GraphEdge = GraphEdge
  { edgeFromHead :: Text
  , edgeToHead :: Text
  , edgeBridgeAddress :: Text
  , edgeFee :: Int64
  }
  deriving stock (Eq, Show)

-- | The precomputed relay graph
data RelayGraph = RelayGraph
  { graphNodes :: Map Text GraphNode -- keyed by head_id
  , graphEdges :: [GraphEdge]
  , graphAdjacency :: Map Text [(Text, GraphEdge)] -- head_id -> [(neighbor_head_id, edge)]
  , graphAddressToHeads :: Map Text (Set Text) -- address -> set of head_ids
  }
  deriving stock (Show)

emptyGraph :: RelayGraph
emptyGraph =
  RelayGraph
    { graphNodes = Map.empty
    , graphEdges = []
    , graphAdjacency = Map.empty
    , graphAddressToHeads = Map.empty
    }

-- | A route found by pathfinding.
--
-- @routeSrcHead@ is the head where the dijkstra started — i.e. the
-- sender's source head. @routeHops@ are the graph edges (each pointing
-- to a destination head + bridging participant). Together they describe
-- the head sequence as @routeSrcHead : map hopHeadId routeHops@. That
-- sequence has @length routeHops + 1@ heads, which is also the number
-- of HTLCs that must be locked along the path (one per head).
data Route = Route
  { routeSrcHead :: Text
  , routeHops :: [RouteHop]
  , routeTotalFee :: Int64
  }
  deriving stock (Eq, Show)

data RouteHop = RouteHop
  { hopHeadId :: Text
  , hopBridgeAddress :: Text
  , hopFee :: Int64
  }
  deriving stock (Eq, Ord, Show)

-- | Build a relay graph from the component data.
--
-- Parameters:
--   - heads: [(headId, network)] — all explorer + locally-registered heads
--   - participants: [(headId, address)] — address→head membership
--
-- Any participant present in two or more heads of the same network forms
-- an edge between every pair of those heads — that participant is the
-- implicit bridge. We no longer require an explicit \"is bridge\"
-- declaration at registration time; running a node in two heads is the
-- declaration. Fees are not encoded in the graph anymore — they belong
-- to a per-payment quote between sender and bridge agent (TBD).
buildGraph ::
  [(Text, Text)] ->
  [(Text, Text)] ->
  RelayGraph
buildGraph heads participants =
  let -- Build nodes
      nodes =
        Map.fromList
          [ ( hid
            , GraphNode
                { nodeHeadId = hid
                , nodeNetwork = net
                , nodeHasHtlc = False
                }
            )
          | (hid, net) <- heads
          ]

      -- Build the full address→heads map. Participants come in two
      -- flavours:
      --   * bech32 Cardano addresses (from the explorer sidecar)
      --   * raw 28-byte vkey-hash hex (from our Indexer's Greetings parsing)
      -- We index both forms so callers can look up by either: bech32 →
      -- also index under derived hex pkh; hex → just itself (we can't
      -- derive bech32 without knowing the network header byte). This
      -- map serves sender/receiver lookup in @findRoutes@ and is kept
      -- unfiltered so popular keys (alice, bob, etc. used by many
      -- testers on Preview) still locate the right heads.
      addrToHeads =
        Map.fromListWith Set.union
          [ (key, Set.singleton hid)
          | (hid, addr) <- participants
          , key <- normalizeAddrKeys addr
          ]

      -- Bridge candidates: addresses that appear in a small group of
      -- heads. The upper bound matters because popular default actor
      -- keys can land in 50+ heads, and treating each as a bridge
      -- between every pair of those heads creates a combinatorial
      -- explosion (millions of edges, Dijkstra hangs). We cap at 10
      -- to discard "everyone has this test key" noise while keeping
      -- real bridges (typically 2–4 head overlap).
      bridgeAddrToHeads =
        Map.filter (\s -> let n = Set.size s in n >= 2 && n <= 10) addrToHeads

      -- For each bridge candidate, create edges between every pair of
      -- heads it sits in (same network only).
      edges =
        [ GraphEdge
            { edgeFromHead = h1
            , edgeToHead = h2
            , edgeBridgeAddress = addr
            , edgeFee = 0
            }
        | (addr, headSet) <- Map.toList bridgeAddrToHeads
        , h1 <- Set.toList headSet
        , h2 <- Set.toList headSet
        , h1 /= h2
        , maybe False (\n1 -> maybe False (\n2 -> n1.nodeNetwork == n2.nodeNetwork) (Map.lookup h2 nodes)) (Map.lookup h1 nodes)
        ]

      -- Build adjacency list
      adjacency =
        Map.fromListWith (++) $
          [(e.edgeFromHead, [(e.edgeToHead, e)]) | e <- edges]
   in RelayGraph
        { graphNodes = nodes
        , graphEdges = edges
        , graphAdjacency = adjacency
        , graphAddressToHeads = addrToHeads
        }

-- | Generate the lookup keys for a participant address. Bech32 inputs are
-- also indexed under their derived 28-byte payment vkey hash hex so
-- queries against either form match. Hex inputs are returned as-is.
normalizeAddrKeys :: Text -> [Text]
normalizeAddrKeys addr = case Htlc.addressOrPkhToBytes addr of
  Right bytes -> [addr, Htlc.hexEncode bytes]
  Left _ -> [addr]

-- | Find up to N cheapest routes from sender to receiver using Dijkstra.
--
-- The sender and receiver are addresses. We look up which heads they're in
-- and find paths between those head sets.
findRoutes :: RelayGraph -> Text -> Text -> Text -> Int -> [Route]
findRoutes graph senderAddr receiverAddr network maxRoutes =
  let lookupAll a =
        Set.unions [Map.findWithDefault Set.empty k graph.graphAddressToHeads | k <- normalizeAddrKeys a]
      senderHeads = filterByNetwork (lookupAll senderAddr)
      receiverHeads = filterByNetwork (lookupAll receiverAddr)
      -- Try all combinations of sender head → receiver head
      allRoutes =
        [ route
        | sh <- Set.toList senderHeads
        , rh <- Set.toList receiverHeads
        , sh /= rh
        , route <- dijkstra graph sh rh
        ]
      -- Sort by total fee and take top N
      sorted = sortByFee allRoutes
   in take maxRoutes sorted
 where
  filterByNetwork :: Set Text -> Set Text
  filterByNetwork = Set.filter $ \hid ->
    case Map.lookup hid graph.graphNodes of
      Just n -> n.nodeNetwork == network
      Nothing -> False

  sortByFee :: [Route] -> [Route]
  sortByFee = map snd . Map.toAscList . Map.fromListWith const . map (\r -> (r.routeTotalFee, r))

-- | Simple Dijkstra from one head to another. Returns at most one route.
dijkstra :: RelayGraph -> Text -> Text -> [Route]
dijkstra graph src dst
  | src == dst = [Route src [] 0]
  | otherwise = go initFrontier Set.empty Map.empty
 where
  initFrontier :: Set (Int64, Text, [RouteHop])
  initFrontier = Set.singleton (0, src, [])

  go ::
    Set (Int64, Text, [RouteHop]) ->
    Set Text ->
    Map Text Int64 ->
    [Route]
  go frontier visited bestCost
    | Set.null frontier = []
    | otherwise =
        let ((cost, current, path), rest) = Set.deleteFindMin frontier
         in if current == dst
              then [Route src (reverse path) cost]
              else
                if Set.member current visited
                  then go rest visited bestCost
                  else
                    let visited' = Set.insert current visited
                        neighbors = Map.findWithDefault [] current graph.graphAdjacency
                        (frontier', bestCost') =
                          foldl
                            ( \(f, bc) (neighbor, edge) ->
                                let newCost = cost + edge.edgeFee
                                    hop =
                                      RouteHop
                                        { hopHeadId = neighbor
                                        , hopBridgeAddress = edge.edgeBridgeAddress
                                        , hopFee = edge.edgeFee
                                        }
                                 in if Set.member neighbor visited
                                      then (f, bc)
                                      else case Map.lookup neighbor bc of
                                        Just prevCost | prevCost <= newCost -> (f, bc)
                                        _ ->
                                          ( Set.insert (newCost, neighbor, hop : path) f
                                          , Map.insert neighbor newCost bc
                                          )
                            )
                            (rest, bestCost)
                            neighbors
                     in go frontier' visited' bestCost'
