module Maximus where

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f [x,y]
  | f x y == GT = x
  | otherwise   = y
myMaximumBy f (x:y:zs)
  | f x y == GT = myMaximumBy f (x:zs)
  | otherwise   = myMaximumBy f (y:zs)

myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy _ [] = undefined
myMinBy _ [x] = x
myMinBy f [x,y]
  | f x y == LT = x
  | otherwise   = y
myMinBy f (x:y:zs)
  | f x y == LT = myMinBy f (x:zs)
  | otherwise   = myMinBy f (y:zs)
