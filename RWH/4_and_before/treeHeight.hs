data Tree a = Node a (Tree a) (Tree a) | Empty

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node a leftBranch rightBranch) = 1 + max (treeHeight leftBranch) (treeHeight rightBranch)
