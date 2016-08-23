data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn i) (TisAn i') = i == i'


data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i j)  (Two i' j') = (i == i') && (j == j')


data StringOrInt =
    TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt i)   (TisAnInt i')   = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _              _               = False


data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = (a == a') && (b == b')
    -- (==) _          _            = False


data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')
    -- (==) _           _             = False


data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    -- (==) _           _            = False


data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    -- (==) _           _            = False










