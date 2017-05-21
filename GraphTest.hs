module GraphTest where

import Graph

-- Find an item in a list
find :: Eq a => a -> [a] -> Bool
find _ [] = False
find x (y:ys)
    | x == y = True
    | otherwise = find x ys

everyNode_ :: [Edge] -> [Node] -> [Node]
everyNode_ [] nodes = nodes
everyNode_ (edge:edges) tmp_nodes
    | (find start_node tmp_nodes) && (find end_node tmp_nodes) = everyNode_ edges tmp_nodes
    | find start_node tmp_nodes = everyNode_ edges (tmp_nodes ++ [end_node])
    | find end_node tmp_nodes = everyNode_ edges (tmp_nodes ++ [start_node])
    -- Did not find nor start node nor end node of the edge
    | otherwise = everyNode_ edges (tmp_nodes ++ [start_node] ++ [end_node])
    where
        start_node = start edge
        end_node = end edge
            

-- Collect a list of every node in a list
-- of all edges. No duplicate nodes in resulting
-- list.
everyNode :: [Edge] -> [Node]
everyNode edges = everyNode_ edges []


testGraph :: String -> Graph
testGraph str = 
    let
        (source_str : sink_str : edges_str) = lines str
        source = nodeFromString source_str
        sink = nodeFromString sink_str
        edges = map edgeFromString edges_str
        nodes = everyNode edges
    in
        (Graph nodes edges source sink)
        

graph1_str :: String
graph1_str = "z \ns \nz u 3 \nz v 1 \nu v 1 \nu s 2\nv s 3"


graph1 :: Graph
graph1 = testGraph graph1_str


graph1_bfs :: Graph
graph1_bfs = (Graph 
    [(Node "z" 0), (Node "u" 1), (Node "v" 1), (Node "s" 2)] -- nodes
    [(Edge (Node "z" 0) (Node "u" 1) 3.0 0 0),
      (Edge (Node "z" 0) (Node "v" 1) 1.0 0 0),
      (Edge (Node "u" 1) (Node "v" 1) 1.0 0 0),
      (Edge (Node "u" 0) (Node "s" 1) 2.0 0 0),
      (Edge (Node "v" 0) (Node "s" 2) 3.0 0 0)] -- edges
    (Node "z" 0) -- source
    (Node "s" 2)) -- sink
    
