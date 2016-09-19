module Cipher where

import Data.Char


caesar :: Int -> String -> String
caesar _ [] = []
caesar shift (x:xs) = t shift x: caesar shift xs
  where t s c
          | c `elem` ['A'..'Z'] = chr $ 65 + (ord c - 65 + s) `mod` 26
          | c `elem` ['a'..'z'] = chr $ 97 + (ord c - 97 + s) `mod` 26

unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar shift (x:xs) = t shift x: caesar shift xs
  where t s c
          | c `elem` ['A'..'Z'] = chr $ 65 + (ord c - 65 - s) `mod` 26
          | c `elem` ['a'..'z'] = chr $ 97 + (ord c - 97 - s) `mod` 26
