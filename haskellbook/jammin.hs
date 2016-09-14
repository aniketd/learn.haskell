module Jammin where

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
    Jam {fruit :: Fruit
        ,jars  :: Int }
    deriving (Eq, Show, Ord)



row1 = [Jam Peach 5, Jam Plum 4, Jam Apple 3, Jam Blackberry 2]
row2 = [Jam Peach 2, Jam Plum 3]
row3 = [Jam Peach 2, Jam Apple 6]
row4 = [Jam Blackberry 10, Jam Plum 7]
row5 = [Jam Plum 15]
row6 = [Jam Apple 9]
allJam = [row1, row2, row3, row4, row5, row6]
