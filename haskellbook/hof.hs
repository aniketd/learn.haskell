module HOF where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (d, _) = x `divMod` 10


foldBool :: a -> a -> Bool -> a
foldBool x _ True = x
foldBool _ y False = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z =
  case z of
    True -> x
    False -> y

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y z
    | z         = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


zip' :: [a] -> [b] -> [(a, b)]
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] ys = []
zipWith' _ xs [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
