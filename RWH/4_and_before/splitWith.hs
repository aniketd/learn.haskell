splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p l = let (pred, succ) = myBreak p l
                in case pred of
                    [] -> case succ of
                            [] -> [[]]
                            _  -> splitWith p succ
                    _  -> pred : case succ of
                                    [] -> []
                                    _  -> splitWith p succ


myBreak p l = (uptill p l, onwards p l)

uptill p [] = []
uptill p (x:xs)
    | p x = x:uptill p xs
    | otherwise = []

onwards p [] = []
onwards p (x:xs)
    | p x = onwards p xs
    | otherwise = xs
