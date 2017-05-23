module GraphTest where

import Graph

-- Create graph from string.
testGraph :: String -> Graph
testGraph str = 
    let
        (source_str : sink_str : edges_str) = lines str
        source = nodeFromString source_str
        sink = nodeFromString sink_str
        edges = map edgeFromString edges_str
    in
        (Graph edges source sink)
        

graph1_str :: String
graph1_str = "z \ns \nz u 3 \nz v 1 \nu v 1 \nu s 2\nv s 3"

graph2_str = "z \ns \nz s 1"


--graph1 :: Graph
--graph1 = testGraph graph1_str


graph1 :: Graph
graph1 = (Graph 
    [(Edge (Node "z" 0) (Node "u" 0) 3.0 0 0),
      (Edge (Node "z" 0) (Node "v" 0) 1.0 0 0),
      (Edge (Node "u" 0) (Node "v" 0) 1.0 0 0),
      (Edge (Node "u" 0) (Node "s" 0) 2.0 0 0),
      (Edge (Node "v" 0) (Node "s" 0) 3.0 0 0)] -- edges
    (Node "z" 0) -- source
    (Node "s" 0)) -- sink



graph2 = testGraph graph2_str


graph2_bfs = (Graph 
    [
        (Edge (Node "z" 0) (Node "u" 1) 1.5 0 0),
        (Edge (Node "u" 1) (Node "s" 2) 2.0 0 0)
    ] 
    (Node "z" 0) 
    (Node "s" 2))


graph1_bfs :: Graph
graph1_bfs = (Graph 
    [(Edge (Node "z" 0) (Node "u" 1) 3.0 0 0),
      (Edge (Node "z" 0) (Node "v" 1) 1.0 0 0),
      (Edge (Node "u" 1) (Node "v" 1) 1.0 0 0),
      (Edge (Node "u" 1) (Node "s" 2) 2.0 0 0),
      (Edge (Node "v" 1) (Node "s" 2) 3.0 0 0)] -- edges
    (Node "z" 0) -- source
    (Node "s" 0)) -- sink
    

graph1_bfs_upgrd = (Graph 
    [(Edge (Node "z" 0) (Node "u" 1) 3.0 2.0 0),
      (Edge (Node "z" 0) (Node "v" 2) 1.0 1.0 0),
      (Edge (Node "u" 1) (Node "v" 2) 1.0 0 0),
      (Edge (Node "u" 1) (Node "s" 3) 2.0 2.0 0),
      (Edge (Node "v" 2) (Node "s" 3) 3.0 1.0 0)] -- edges
    (Node "z" 0) -- source
    (Node "s" 3)) -- sink