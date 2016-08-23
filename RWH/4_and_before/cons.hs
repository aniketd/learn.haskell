data List a = Cons a (List a) | Nil deriving (Show)

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

data ATree a = ANode a (Maybe (ATree a)) (Maybe (ATree a)) deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList :: List a -> [a]
toList (Cons x xs) = x:toList xs
toList Nil = []
