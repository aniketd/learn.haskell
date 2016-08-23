intersperse :: a -> [[a]] -> [a]
intersperse s (x:[]) = x
intersperse s (x:xs) = x ++ [s] ++ intersperse s xs
intersperse _ _      = []

