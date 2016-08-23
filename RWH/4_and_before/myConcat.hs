myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat l = foldr step [] l
    where step x acc = x ++ acc
