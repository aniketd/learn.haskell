module Unfold where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- take 10 $ unfoldr (\b -> Just (b, b+1)) 0
-- >>> [0,1,2,3,4,5,6,7,8,9]
-- here, the b + 1 is obviously the fodder for the next call
myUnfoldr f b = case f b of
                  Just (member, fodder) -> member : myUnfoldr f fodder
                  Nothing               -> []

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing          -> Leaf
  Just (n1, b, n2) -> Node (unfold f n1) b (unfold f n2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild n = Node (treeBuild (n-1)) 0 (treeBuild (n-1))
