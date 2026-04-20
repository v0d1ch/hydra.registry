module Relay.Graph where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

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

-- | A route found by pathfinding
data Route = Route
  { routeHops :: [RouteHop]
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
--   - heads: [(headId, network)] — all explorer heads
--   - participants: [(headId, address)] — address→head membership
--   - bridgeHeadIds: Set of head_ids that are registered as bridges
--   - bridgeFees: headId → fee in lovelace
--   - htlcHeadIds: Set of head_ids that have the HTLC script
buildGraph ::
  [(Text, Text)] ->
  [(Text, Text)] ->
  Set Text ->
  Map Text Int64 ->
  Set Text ->
  RelayGraph
buildGraph heads participants bridgeHeadIds bridgeFees htlcHeadIds =
  let -- Build nodes
      nodes =
        Map.fromList
          [ ( hid
            , GraphNode
                { nodeHeadId = hid
                , nodeNetwork = net
                , nodeHasHtlc = Set.member hid htlcHeadIds
                }
            )
          | (hid, net) <- heads
          ]

      -- Build address→heads map
      addrToHeads =
        Map.fromListWith Set.union $
          [(addr, Set.singleton hid) | (hid, addr) <- participants]

      -- Build edges: for each address in 2+ heads where at least one head is a bridge,
      -- create edges between all pairs of those heads
      edges =
        [ GraphEdge
            { edgeFromHead = h1
            , edgeToHead = h2
            , edgeBridgeAddress = addr
            , edgeFee = Map.findWithDefault 0 h1 bridgeFees
            }
        | (addr, headSet) <- Map.toList addrToHeads
        , Set.size headSet >= 2
        , h1 <- Set.toList headSet
        , h2 <- Set.toList headSet
        , h1 /= h2
        , -- At least one of the heads must be a bridge
          Set.member h1 bridgeHeadIds || Set.member h2 bridgeHeadIds
        , -- Both heads must have the HTLC script
          Set.member h1 htlcHeadIds
        , Set.member h2 htlcHeadIds
        , -- Same network
          maybe False (\n1 -> maybe False (\n2 -> n1.nodeNetwork == n2.nodeNetwork) (Map.lookup h2 nodes)) (Map.lookup h1 nodes)
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

-- | Find up to N cheapest routes from sender to receiver using Dijkstra.
--
-- The sender and receiver are addresses. We look up which heads they're in
-- and find paths between those head sets.
findRoutes :: RelayGraph -> Text -> Text -> Text -> Int -> [Route]
findRoutes graph senderAddr receiverAddr network maxRoutes =
  let senderHeads = filterByNetwork $ Map.findWithDefault Set.empty senderAddr graph.graphAddressToHeads
      receiverHeads = filterByNetwork $ Map.findWithDefault Set.empty receiverAddr graph.graphAddressToHeads
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
  | src == dst = [Route [] 0]
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
              then [Route (reverse path) cost]
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
