module SumAll where

sumAll :: (Eq a, Num a) => a -> a
sumAll n = go 0 n
    where go acc 0 = acc
          go acc n = go (acc + n) (n - 1)


mulBySum :: (Integral a) => a -> a -> a 
mulBySum 0 0 = 0
mulBySum 0 n = 0
mulBySum m 0 = 0
mulBySum m 1 = m
mulBySum 1 n = n
mulBySum m n = go 0 m n
    where go acc m 0 = acc
          go acc m n = go (acc + m) m (n - 1)
    

data DividedResult a = Result a a
                   | DividedByZero
                   deriving (Eq, Show)

divBy :: (Integral a) => a -> a -> DividedResult a
divBy num denom = go num denom 0 
    where go n d count
                | d == 0        = DividedByZero
                | n < d         = Result count n
                | otherwise     = go (n - d) d (count + 1)

-- TODO: Complete this fix. Learn some math.


mc91 :: Integer -> Integer
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91(mc91(n + 11))
