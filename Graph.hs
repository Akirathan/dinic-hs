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
    nodes :: [Node], 
    edges :: [Edge],
    source :: Node,
    sink :: Node
} deriving Show
    

nodeNeighbours :: Graph -> Node -> [Node]
nodeNeighbours graph@(Graph nodes _ _ _) node =
    let
        edges = edgeNeighbours graph node
    in
        map end edges
    
    
edgeNeighbours :: Graph -> Node -> [Edge]
edgeNeighbours (Graph nodes edges _ _) node_name =
    filter (\e -> ((start e) == node_name)) edges
    

changeNodeLevel :: [Node] -> Node -> Int -> [Node]
changeNodeLevel [] _ _ = []
changeNodeLevel (node:nodes) node_to_change node_lvl
    -- Insert Node with new node_lvl
    | node == node_to_change = ((Node (name node) node_lvl):nodes)
    -- Recursive call
    | otherwise = (node:changeNodeLevel nodes node_to_change node_lvl)

    
    
