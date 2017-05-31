-- Main source file with the definition of Dinic algorithm.
--
-- The topmost dinic function works in steps where every step consists of bfs and dfs parts. 
-- The bfs part is basically BFS algorithm called on the input graph that associates level 
-- to every node in the graph and checks whether the sink node can be reached from the source node.
--
-- The dfs part searches for augmenting paths and upgrades the flow on these paths. This part 
-- ends when there is no augmenting path left. Searching of augmenting paths is done via dfs 
-- function which is splitted into two functions: dfsNode and dfsEdge.
-- 
-- DfsNode just chooses into which path (on which neighboring node) should the algorithm continue. 
-- This function recursively calls itself (more precisely it calls dfsEdge function) on every 
-- neighboring edge that has some reserve. Here lazy evaluation is taken advantage of because the 
-- algorithm does not proceed into every appropriate neighboring edge, just into the first one.

module Dinic where

import Graph

-- This data structure is created
-- while dfs is running. It represents
-- an augmenting path in the graph.
data FlowPath = FlowPath {
    pathEdges :: [Edge],
    pathFlow :: Float
} deriving (Show, Eq)


dinic :: Graph -> Graph
dinic graph = dinic_ graph True

-- Params:
-- 1) Graph
-- 2) Bool flag whether next step should
-- be done. This flag is copied from step
-- function.
dinic_ :: Graph -> Bool -> Graph
-- Stop execution
dinic_ graph False = graph
-- Continue
dinic_ graph True =
    let
        ret_pair = step graph
        new_graph = fst ret_pair
        can_continue = snd ret_pair
    in
        if can_continue then
            dinic_ new_graph True
        else
            dinic_ new_graph False


-- Represents one step of the dinic algorithm.
-- Executes bfs and then dfs on the graph. In other words
-- first divide the graph into layers and then upgrade flow
-- on every augmenting path.
-- The Bool returned from this function is flag whether this
-- step can be called again.
step :: Graph -> (Graph, Bool)
step graph@(Graph edges src_node sink_node) =
    let
        -- Every node in this graph has level
        layered_graph = fst $ bfs graph
        sink_reachable = snd $ bfs graph
    in
        if sink_reachable then
            (dfs layered_graph, True)
        else
            -- Return unchanged graph because sink
            -- cannot be reached.
            (graph, False)
 


-- Add node to the queue with specified index (level).
-- Params:
-- 1) Current queue
-- 2) The list of already visited nodes
-- 3) Index that should be paired with newly added nodes
-- 4) Nodes to be added to the queue
addToQueue :: [(Node, Int)] -> [Node] -> Int -> [Node] -> [(Node, Int)]
-- Iterate over nodes_to_add, return curr_queue
addToQueue curr_queue _ _ [] = curr_queue
addToQueue curr_queue visited_nodes idx (node_to_add:nodes_to_add)
    -- Check if node_to_add is in visited_nodes or in curr_queue
    | node_to_add `elem` visited_nodes || node_to_add `elem` map fst curr_queue =
        -- Don't enque this node_to_add into curr_queue
        addToQueue curr_queue visited_nodes idx nodes_to_add
    | otherwise =
        -- Enque node_to_add with idx and insert it into visited_nodes
        addToQueue (curr_queue ++ [(node_to_add, idx)]) (visited_nodes ++ [node_to_add]) idx nodes_to_add


-- Return appropriate neighboring edges for given node
-- in bfs part. Considers also edges in opposite 
-- direction ie. edges in opposite direction that have 
-- some flow can be upgraded.
-- Params:
-- 1) Starting node
-- 2) List of edges
bfsAppropriateEdges :: Node -> [Edge] -> [Edge]
bfsAppropriateEdges start_node edges = 
    filter (f start_node) edges
    where
        f start_node (Edge edge_start_node edge_end_node capacity_e flow_e)
            -- Edge in right direction
            | start_node == edge_start_node =
                -- Check for reserve
                flow_e < capacity_e 
            
            -- Edge in opposite direction
            | otherwise =
                flow_e > 0


-- Assign level to every node in the graph.
-- Params:
-- 1) Input graph
-- 2) Source node
-- Returns: pair where the first element is graph and
-- the second is bool value whether the sink node
-- can be reached from the source node.
bfs :: Graph -> (Graph, Bool)
bfs graph = 
    -- Initialize the queue with source node with level 0
    bfs_ graph [] [(source graph, 0)] False


-- 1) Graph
-- 2) List of visited nodes
-- 3) Queue
-- 4) Bool flag whether sink was already reached
-- Return: Graph where nodes have associated level and
-- bool flag whether sink node was already reached.
bfs_ :: Graph -> [Node] -> [(Node, Int)] -> Bool -> (Graph, Bool)
-- The queue is empty, return the changed graph
-- and bool flag.
bfs_ g _ [] bool = (g, bool)
bfs_ graph@(Graph edges src_node sink_node) visited_list ((node, node_idx):queue_rest) bool =
   let 
        -- Change level of the current node
        new_edges = changeNodeLevel edges node node_idx
        
        -- Get all the edge neighbours of the current node
        edge_neighbours = edgeNeighbors graph node
        
        -- Filter only edges that can be upgraded
        appropriate_edge_neighbours = bfsAppropriateEdges node edge_neighbours
        appropriate_node_neighbours = map f appropriate_edge_neighbours
        
        f edge 
            | (end edge) /= node = (end edge)
            | otherwise = (start edge)
        
        -- Create new queue. None of the added nodes should be in
        -- visited_list. Every added node to the queue will be 
        -- added as a pair with index (node_idx+1).
        new_queue = addToQueue queue_rest visited_list (node_idx+1) appropriate_node_neighbours
        
        -- Add current node to visited_list
        new_visited_list = visited_list ++ [node]
    in
        -- If sink node was already reached, or if sink node is in the 
        -- queue (note that if node is in appropriate_node_neighbours list
        -- then it is also in the queue).
        if bool || elem sink_node appropriate_node_neighbours then
            bfs_ (Graph new_edges src_node sink_node) new_visited_list new_queue True
        else
            bfs_ (Graph new_edges src_node sink_node) new_visited_list new_queue False


{-==================================================================-}
{-                           DFS part                               -}
{-==================================================================-}

-- Increases flow in one edge in the list of edges.
incFlow :: [Edge] -> Edge -> Float -> [Edge]
incFlow edges edge@(Edge start_edge end_edge cap_edge flow_edge) flow_ =
    head_edges ++ [Edge start_edge end_edge cap_edge (flow_edge + flow_)] ++ tail_edges
    where
        head_edges = takeWhile (/=edge) edges
        tail_edges = tail $ dropWhile (/=edge) edges


-- Decreases flow in one edge in the list of edges.
decFlow :: [Edge] -> Edge -> Float -> [Edge]
decFlow edges edge@(Edge start_edge end_edge cap_edge flow_edge) flow_ =
    head_edges ++ [Edge start_edge end_edge cap_edge (flow_edge - flow_)] ++ tail_edges
    where
        head_edges = takeWhile (/=edge) edges
        tail_edges = tail $ dropWhile (/=edge) edges


upgradePath :: Graph -> FlowPath -> Graph
upgradePath graph flow_path@(FlowPath path_edges flow_) = 
    upgradePath_ graph (source graph) flow_path 


-- Considers also upgrading flow in "opposite" edges.
-- Params:
-- 1) Graph
-- 2) Last node. This param is needed so the opposite
-- edges in the flow path are recognized.
-- 3) FlowPath
upgradePath_ :: Graph -> Node -> FlowPath -> Graph
-- Iterate over edges of the FlowPath
upgradePath_ graph _ (FlowPath [] _) = graph
upgradePath_ graph last_node (FlowPath (path_edge:path_edges) flow_upgrade)
    -- Next edge is in right direction
    | last_node == start path_edge = 
        -- Copy graph with modified edges to the recursive call
        upgradePath_ (Graph new_edges_inc (source graph) (sink graph)) (end path_edge) new_flow_path
        
    | otherwise =
        -- Copy graph with modified edges to the recursive call
        upgradePath_ (Graph new_edges_dec (source graph) (sink graph)) (start path_edge) new_flow_path 
        
        where
            new_edges_dec = decFlow (edges graph) path_edge flow_upgrade
            new_edges_inc = incFlow (edges graph) path_edge flow_upgrade
            -- FlowPath without head
            new_flow_path = (FlowPath path_edges flow_upgrade)


-- Filters appropriate neighboring edges for dfs part. Checks
-- reserve in edges and for correct levels of nodes.
-- Params:
-- 1) Starting node
-- 2) List of edges
dfsAppropriateEdges :: Node -> [Edge] -> [Edge]
dfsAppropriateEdges start_node edges = 
    filter (f start_node) edges
    where
        f start_node (Edge edge_start_node edge_end_node capacity_e flow_e)
            -- Edge in right direction
            | start_node == edge_start_node =
                -- Check for reserve and level
                flow_e < capacity_e && level edge_end_node == ((level edge_start_node) + 1)
        
            -- Edge in opposite direction
            | otherwise =
                flow_e > 0 && level edge_start_node == ((level edge_end_node) + 1)


-- Until there is an augmenting path from source to sink,
-- sends maximal flow through that path. Suppose that bfs
-- was already executed, so the nodes' levels are correct,
-- otherwise no path can be upgraded.
dfs :: Graph -> Graph
dfs graph@(Graph edges src_node sink_node) =
    let
        -- Try to find an augmenting path
        ret_pair = dfsNode graph src_node 
        flow_path = fst ret_pair
        sink_reached = snd ret_pair
    in
        if sink_reached then
            let
                upgraded_graph = upgradePath graph flow_path
            in
                -- Recursive step
                dfs upgraded_graph
        else
            -- Sink was not reached. Return
            -- passed graph and end.
            graph
        

-- Filters every neighboring edge and chooses where to continue. 
-- Returns FlowPath structure and bool flag whether this FlowPath
-- structure contains the sink node. 
-- Note that if the sink node is in FlowPath then this path is 
-- considered as augmenting.
dfsNode :: Graph -> Node -> (FlowPath, Bool)
dfsNode graph node@(Node node_name node_lvl) =
    let
        neighbour_edges = edgeNeighbors graph node
        -- Choose the edge when there is a node with higher level on
        -- the end and that has some reserve. Check also for edges in
        -- opposite direction.
        appropriate_edges = dfsAppropriateEdges node neighbour_edges
    in
        -- Check if there are any appropriate edges, if there are not,
        -- then the sink is unreachable ie. no path can be upgraded.
        if null appropriate_edges then
            (FlowPath [] 0, False)
        else
            let
                results = map (dfsEdge graph node) appropriate_edges
                -- Drop all the false results (paths that are not augmenting)
                -- and return the first true result. 
                true_results = dropWhile (not.snd) results
            in
                -- Check if there is any path that
                -- can be upgraded.
                if null true_results then
                    (FlowPath [] 0, False)
                else
                    head true_results


-- Considers also upgrading in opposite directions, ie flow in edge in opposite
-- direction can be upgraded by decreasing the flow in this edge.
-- Params:
-- 1) Graph
-- 2) Start node
dfsEdge :: Graph -> Node -> Edge -> (FlowPath, Bool)
dfsEdge graph start_node edge
    -- Edge in right direction
    | start_node == start edge =
        right_direction graph edge
        
    -- Edge in opposite direction
    | otherwise =
        opposite_direction graph edge
        
        where
            right_direction graph edge
                -- Check whether sink was reached
                | end edge == sink graph = (FlowPath [edge] (capacity edge - flow edge), True)
                -- Check whether this edge is tight throat
                | capacity edge - flow edge < max_flow_r = (FlowPath new_path_r (capacity edge - flow edge), bool_r)
                | otherwise = (FlowPath new_path_r max_flow_r, bool_r)

            opposite_direction graph edge
                -- Check whether sink was reached
                | start edge == sink graph = (FlowPath [edge] (flow edge), True)
                -- Check whether this edge is tight throat
                | flow edge > 0 = (FlowPath new_path_o (flow edge), bool_o)
                | otherwise = (FlowPath new_path_o max_flow_o, bool_o)
            
            -- Recursive call definitions for edge in right direction
            ret_pair_r = dfsNode graph (end edge)
            max_flow_r = pathFlow (fst ret_pair_r)
            path_r = pathEdges (fst ret_pair_r)
            bool_r = snd ret_pair_r
            -- Path to be returned
            new_path_r = (edge:path_r)
            
            -- Recursive call definitions for edge in opposite direction
            ret_pair_o = dfsNode graph (start edge)
            max_flow_o = pathFlow (fst ret_pair_o)
            path_o = pathEdges (fst ret_pair_o)
            bool_o = snd ret_pair_o
            -- Path to be returned
            new_path_o = (edge:path_o)
