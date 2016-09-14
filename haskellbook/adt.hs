{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
    tooMany (i, s) = i > 42

instance TooMany (Int, Int) where
    tooMany (i, s) = i + s > 42

instance (Num a) => TooMany (a, a) where
    tooMany (a, b) = a + b > (42 :: Num)

newtype Men = Men (Int, String) deriving (Eq, Show, TooMany)
