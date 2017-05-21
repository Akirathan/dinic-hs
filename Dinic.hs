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
}

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


-- Assign level to every node in the graph.
-- During following dfs, edges within the same
-- level will be ignored.
bfs :: Graph -> Graph
bfs graph = 
    let
        src_node = source graph
    in
        -- Initialize the queue with source node with level 0
        bfs_ graph [] [(src_node, 0)]


-- 1) Graph
-- 2) List of visited nodes
-- 3) Queue
bfs_ :: Graph -> [Node] -> [(Node, Int)] -> Graph
bfs_ g _ [] = g
bfs_ graph@(Graph nodes edges src_node sink_node) visited_list ((node, node_idx):queue_rest) =
   let 
        -- Change level of the current node
        new_nodes = changeNodeLevel nodes node node_idx
        
        -- Get all the neighbours of the current node
        neighbours = nodeNeighbours graph node
        
        -- Create new queue. None of the added nodes should be in
        -- visited_list. Every added node to the queue will be 
        -- added as a pair with index (node_idx+1).
        new_queue = addToQueue queue_rest visited_list (node_idx+1) neighbours
        
        -- Add current node to visited_list
        new_visited_list = visited_list ++ [node]
    in
        bfs_ (Graph new_nodes edges src_node sink_node) new_visited_list new_queue


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
upgradePath (Graph graph_nodes graph_edges graph_source graph_sink) (FlowPath (path_edge:path_edges) flow_upgrade) =
    let
        -- Upgrade every edge in the graph
        new_graph_edges = upgradeEdge graph_edges path_edge flow_upgrade
    in
        -- Recursive step on graph with upgraded edges (specifically just
        -- one edge of the whole path was upgraded).
        upgradePath (Graph graph_nodes new_graph_edges graph_source graph_sink) (FlowPath path_edges flow_upgrade)
    


-- Until there is a non-blocking path from source to sink,
-- send maximal flow through that path. Suppose that bfs
-- was already executed, so the nodes' levels are correct.
dfs :: Graph -> Graph
dfs graph@(Graph _ _ src_node _) =
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
        
        -- Recursive step
        next_edge = head appropriate_edges
    in
        -- Check if there are any appropriate edges, if there are not,
        -- then the sink is unreachable ie. no path can be upgraded.
        if appropriate_edges == [] then
            ((FlowPath [] 0), False)
        else
            dfsEdge graph next_edge



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






