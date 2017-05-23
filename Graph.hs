module Graph where


data Node = Node {
    name :: String,
    level :: Int
}

instance Show Node where
    show (Node n lvl) = "("++n++", "++(show lvl)++")"


instance Eq Node where
    node1 == node2 = name node1 == name node2 

    
data Edge = Edge {
    start :: Node,
    end :: Node,
    capacity :: Float,
    flow :: Float,
    reserve :: Float -- TODO: put reserve away?
}

instance Eq Edge where
    (Edge start_node1 end_node1 _ _ _) == (Edge start_node2 end_node2 _ _ _) =
        start_node1 == start_node2 && end_node1 == end_node2


instance Show Edge where
    show (Edge start_node end_node c f r) = 
        "("++(show start_node)++"->"++(show end_node)++", c="++(show c)++", f="++(show f)++", r="++(show r)++")"


edgeFromString :: String -> Edge
edgeFromString str = 
    let
        (s:e:c:_) = words str
    in
        (Edge (nodeFromString s) (nodeFromString e) (read c::Float) 0 0)

    
nodeFromString :: String -> Node
nodeFromString str = (Node str 0)


data Graph = Graph {
    edges :: [Edge],
    source :: Node,
    sink :: Node
} deriving Show
    

nodeNeighbours :: Graph -> Node -> [Node]
nodeNeighbours graph node =
    let
        edges = edgeNeighbours graph node
    in
        map end edges
    
    
edgeNeighbours :: Graph -> Node -> [Edge]
edgeNeighbours (Graph edges _ _) node_name =
    filter (\e -> ((start e) == node_name)) edges
    

-- Change node level in list of edges (all edges of the
-- graph). Change level of the node on all occurences.
changeNodeLevel :: [Edge] -> Node -> Int -> [Edge]
changeNodeLevel [] _ _ = []
changeNodeLevel (edge@(Edge start_node end_node cap f r):edges) node_to_change node_lvl
    -- Starting node of the edge should be changed
    | start_node == node_to_change = 
        ([(Edge (Node (name node_to_change) node_lvl) end_node cap f r)] ++ changeNodeLevel edges node_to_change node_lvl)
    
    -- Ending node of the edge should be changed
    | end_node == node_to_change = 
        ([(Edge start_node (Node (name node_to_change) node_lvl) cap f r)] ++ changeNodeLevel edges node_to_change node_lvl)
    
    -- Recursive call
    | otherwise = ([edge] ++ changeNodeLevel edges node_to_change node_lvl)

    
    
