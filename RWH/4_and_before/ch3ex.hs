length' :: [a] -> Int
length' (x:xs) = 1 + length' xs
length' [] = 0

sum' :: Num a => [a] -> a
sum' (x:xs) = x + sum' xs
sum' [] = 0

mean' :: Fractional a => [a] -> a
mean' (x:xs) = sum' (x:xs) / (fromIntegral $ length' (x:xs))

mkPalindrome :: [a] -> [a]
mkPalindrome (x:xs) = x:mkPalindrome xs ++ [x]
mkPalindrome [] = []

isPalindrome :: Eq a => [a] -> Bool -- adding a constraint for Eq made it
isPalindrome xs = xs == (reverse xs)
