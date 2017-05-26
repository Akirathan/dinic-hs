module Graph where

data Node = Node {
    name :: String,
    level :: Int
}

data Edge = Edge {
    start :: Node,
    end :: Node,
    capacity :: Float,
    flow :: Float
}

data Graph = Graph {
    edges :: [Edge],
    source :: Node,
    sink :: Node
} deriving Show


instance Show Node where
    show (Node n lvl) = n


instance Eq Node where
    node1 == node2 = name node1 == name node2 


-- Two edges are considered equal if they
-- have the same starting and ending nodes.
instance Eq Edge where
    (Edge start_node1 end_node1 _ _) == (Edge start_node2 end_node2 _ _) =
        start_node1 == start_node2 && end_node1 == end_node2


instance Show Edge where
    show (Edge start_node end_node c f) = 
        "("++(show start_node)++"->"++(show end_node)++", c="++(show c)++", f="++(show f)++")"


edgeFromString :: String -> Edge
edgeFromString str = 
    let
        (s:e:c:_) = words str
    in
        (Edge (nodeFromString s) (nodeFromString e) (read c::Float) 0)


nodeFromString :: String -> Node
nodeFromString str = (Node str 0)


-- Create graph from string.
graphFromString :: String -> Graph
graphFromString str = 
    let
        (source_str : sink_str : edges_str) = lines str
        source = nodeFromString source_str
        sink = nodeFromString sink_str
        edges = map edgeFromString edges_str
    in
        (Graph edges source sink)

    
edgeNeighbors :: Graph -> Node -> [Edge]
edgeNeighbors (Graph edges _ _) node_name =
    filter (\e -> ((start e) == node_name)) edges
    

-- Change node level in list of edges (all edges of the
-- graph). Change level of the node on all occurences.
-- Params:
-- 1) List of edges
-- 2) Node to change
-- 3) Level that should be assigned to the node
changeNodeLevel :: [Edge] -> Node -> Int -> [Edge]
changeNodeLevel [] _ _ = []
changeNodeLevel (edge@(Edge start_node end_node c f):edges) node_to_change node_lvl
    -- Starting node of the edge should be changed
    | start_node == node_to_change = 
        ([(Edge (Node (name node_to_change) node_lvl) end_node c f)] ++ changeNodeLevel edges node_to_change node_lvl)
    
    -- Ending node of the edge should be changed
    | end_node == node_to_change = 
        ([(Edge start_node (Node (name node_to_change) node_lvl) c f)] ++ changeNodeLevel edges node_to_change node_lvl)
    
    -- Proceed to the rest of the list
    | otherwise = ([edge] ++ changeNodeLevel edges node_to_change node_lvl)

    
    
