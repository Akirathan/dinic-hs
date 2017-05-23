module Dinic where

import Graph
import GraphTest

-- This data structure is created
-- while dfs is running. It represents
-- the path in the graph to be
-- upgraded.
data FlowPath = FlowPath {
    pathEdges :: [Edge],
    pathFlow :: Float
} deriving (Show, Eq)


dinic :: Graph -> Graph
dinic graph = dinic_ graph True

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


-- Execute bfs and then dfs on the graph. In other words
-- first divide the graph into layers and then upgrade flow
-- in every path possible.
-- The Bool returned from this function is flag whether this
-- step can be called again.
step :: Graph -> (Graph, Bool)
step graph@(Graph edges src_node sink_node) =
    let
        -- Every node in this graph has level
        indexed_graph = fst $ bfs graph
        sink_reachable = snd $ bfs graph
    in
        if sink_reachable then
            (dfs indexed_graph, True)
        else
            -- Return unchanged graph because sink
            -- cannot be reached.
            (graph, False)
 


-- Add node to the queue with specified index (level).
-- 1) Current queue
-- 2) The list of already visited nodes
-- 3) Index that should be paired with newly added nodes
-- 4) Nodes to be added to the queue
addToQueue :: [(Node, Int)] -> [Node] -> Int -> [Node] -> [(Node, Int)]
-- Iterate over nodes_to_add, return curr_queue
addToQueue curr_queue _ _ [] = curr_queue
addToQueue curr_queue visited_nodes idx (node_to_add:nodes_to_add)
    -- Check if node_to_add is in visited_nodes or in curr_queue
    | (elem node_to_add visited_nodes) || (elem node_to_add (map fst curr_queue)) =
        -- Don't enque this node_to_add into curr_queue
        addToQueue curr_queue visited_nodes idx nodes_to_add
    | otherwise =
        -- Enque node_to_add with idx and insert it into visited_nodes
        addToQueue (curr_queue ++ [(node_to_add, idx)]) (visited_nodes ++ [node_to_add]) idx nodes_to_add


-- TODO: does not work when called as
-- bfs graph1 (source graph1), but works
-- when called as bfs graph1 (Node "z" 0)
--
-- Assign level to every node in the graph.
-- During following dfs, edges within the same
-- level will be ignored.
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
bfs_ :: Graph -> [Node] -> [(Node, Int)] -> Bool -> (Graph, Bool)
-- The queue is empty, return the changed graph
-- and bool flag.
bfs_ g _ [] bool = (g, bool)
bfs_ graph@(Graph edges src_node sink_node) visited_list ((node, node_idx):queue_rest) bool =
   let 
        -- Change level of the current node
        new_edges = changeNodeLevel edges node node_idx
        
        -- Get all the edge neighbours of the current node
        edge_neighbours = edgeNeighbours graph node
        
        -- Filter only edges that can be upgraded
        appropriate_edge_neighbours = filter (\edge -> ((capacity edge) - (flow edge)) > 0) edge_neighbours
        appropriate_node_neighbours = map end appropriate_edge_neighbours
        
        -- Create new queue. None of the added nodes should be in
        -- visited_list. Every added node to the queue will be 
        -- added as a pair with index (node_idx+1).
        new_queue = addToQueue queue_rest visited_list (node_idx+1) appropriate_node_neighbours
        
        -- Add current node to visited_list
        new_visited_list = visited_list ++ [node]
    in
        -- If sink node was already reached, or if sink node is in the 
        -- queue (note that if node is in appropriate_node_neighbours list
        -- then it is also in queue).
        if (bool || elem sink_node appropriate_node_neighbours) then
            bfs_ (Graph new_edges src_node sink_node) new_visited_list new_queue True
        else
            bfs_ (Graph new_edges src_node sink_node) new_visited_list new_queue False


-- Upgrade flow in one edge
upgradeEdge_ :: Edge -> Float -> Edge
upgradeEdge_ (Edge start end capacity flow reserve) flow_ = 
    (Edge start end capacity (flow + flow_) reserve)

-- Upgrade one edge in list of edges
upgradeEdge :: [Edge] -> Edge -> Float -> [Edge]
upgradeEdge [] _ _ = [] -- Unreachable code
upgradeEdge (x:edges) edge flow 
    -- The edge to be upgraded was reached => upgrade it
    | x == edge = [(upgradeEdge_ edge flow)] ++ edges
    | otherwise = [x] ++ (upgradeEdge edges edge flow)

    
-- Upgrade one path in the graph.
upgradePath :: Graph -> FlowPath -> Graph
-- Iterate over edges of the FlowPath
upgradePath graph (FlowPath [] _) = graph
upgradePath (Graph graph_edges graph_source graph_sink) (FlowPath (path_edge:path_edges) flow_upgrade) =
    let
        -- Upgrade every edge in the graph
        new_graph_edges = upgradeEdge graph_edges path_edge flow_upgrade
    in
        -- Recursive step on graph with upgraded edges (specifically just
        -- one edge of the whole path was upgraded).
        upgradePath (Graph new_graph_edges graph_source graph_sink) (FlowPath path_edges flow_upgrade)
    


-- Until there is a non-blocking path from source to sink,
-- send maximal flow through that path. Suppose that bfs
-- was already executed, so the nodes' levels are correct.
dfs :: Graph -> Graph
dfs graph@(Graph edges src_node sink_node) =
    let
        -- Try to find a path that can be upgraded
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
        


dfsNode :: Graph -> Node -> (FlowPath, Bool)
dfsNode graph node@(Node node_name node_lvl) =
    let
        neighbour_edges = edgeNeighbours graph node
        -- Choose the edge when there is a node with higher level on
        -- the end and that has some reserve.
        appropriate_edges = filter (\e -> (flow e < capacity e) && (level (end e) > level node)) neighbour_edges
    in
        -- Check if there are any appropriate edges, if there are not,
        -- then the sink is unreachable ie. no path can be upgraded.
        if appropriate_edges == [] then
            ((FlowPath [] 0), False)
        else
            let
                results = map (dfsEdge graph) appropriate_edges
                -- Drop all the false results (paths that cannot be upgraded)
                -- and return the first true result (ie. first path that can
                -- be upgraded).
                true_results = dropWhile (\pair -> (snd pair) == False) results
            in
                -- Check if there is any path that
                -- can be upgraded.
                if true_results == [] then
                    ((FlowPath [] 0), False)
                else
                    head true_results



-- DFS step called on an appropriate edge,
-- ie. the next node was already chosen.
dfsEdge :: Graph -> Edge -> (FlowPath, Bool)
dfsEdge graph edge
    -- The sink was reached, return true
    | end edge == sink graph = ((FlowPath new_path ((capacity edge) - (flow edge))), True)
    | capacity edge < max_flow = ((FlowPath new_path (capacity edge)), bool)
    | otherwise = ((FlowPath new_path max_flow), bool)
    where
        ret_pair = dfsNode graph (end edge)
        max_flow = pathFlow (fst ret_pair)
        path = pathEdges (fst ret_pair)
        bool = snd ret_pair
        
        -- Path to be returned
        new_path = path ++ [edge]






