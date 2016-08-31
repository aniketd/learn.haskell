module Cipher where

import Data.Char


caesar :: Int -> String -> String
caesar _ [] = []
caesar shift (x:xs) = t shift x: caesar shift xs
  where t s c = chr $ (+s) $ 97 + (ord c - 97) `rem` 26

unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar shift (x:xs) = t shift x: caesar shift xs
  where t s c = chr $ (97 + (ord c - 97) `rem` 26) - s
