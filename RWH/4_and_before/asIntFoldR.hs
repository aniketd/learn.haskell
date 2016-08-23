import Data.Char (digitToInt)
asInt :: String -> Int
asInt s = foldInt 0 s
    where foldInt acc s = foldl (\acc x -> acc * 10 + (digitToInt x)) acc s
