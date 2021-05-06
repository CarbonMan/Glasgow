-- | Insert a value into the tree at the root
insertNode :: Int -> Tree ->Tree
-- | Replace the leaf with a node
insertNode i Leaf = Node i Leaf Leaf
insertNode i (Node x lhs rhs)  
    | i == x  = Node x lhs rhs
    | i > x   = Node x lhs (balanceNode i (Node x lhs rhs) rhs)
    | i < x   = Node x (balanceNode i (Node x lhs rhs) lhs) rhs

-- | If inserting a value into a branch look to balance it.
-- | The parent and child Nodes are passed in
balanceNode :: Int -> Tree -> Tree ->Tree
-- | Replace the leaf with a node
balanceNode i (Node p pLhs pRhs) Leaf   = Node i Leaf Leaf
balanceNode i (Node p pLhs pRhs) (Node c lhs rhs)  
    -- | If the insert value should sit between the parent and child nodes then it is inserted
    | i<p && i > c   = Node i pLhs Leaf
    | i>p && i < c   = Node i Leaf pRhs
    -- | Attach to the child
    | i > c          = Node c lhs (balanceNode i (Node c lhs rhs) rhs)
    | i < c          = Node c (balanceNode i (Node c lhs rhs) lhs) rhs
    -- | Equal to the child. Child stays.
    | otherwise      = Node c lhs rhs

treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node x lhs rhs) = treeToList lhs ++ [x] ++ treeToList rhs
