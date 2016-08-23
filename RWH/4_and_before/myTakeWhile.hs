myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs)
    | p x       = x : myTakeWhile p xs
    | otherwise = []

myTakeWhile_fold :: (a -> Bool) -> [a] -> [a]
myTakeWhile_fold p [] = []
myTakeWhile_fold p l = foldr step [] l
    where step x acc
                | p x = x:acc
                | otherwise = acc
