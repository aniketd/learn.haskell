module VigCiphere where

import Data.Char (ord, chr)

type Key = String
type Payload = String

-- Having `Key` and `Payload` as aliases clears up
-- which parameter means what in the signature.
vignere :: Key -> Payload -> String
vignere _ "" = ""
vignere k p  = map (fst . encode) zipped
  where zipped = zipWithoutSpaces p $ cycle k
        encode (' ', _) = (' ', ' ')
        encode (x, y)   = (shift y x, y)
        shift key char
          | char `elem` ['A'..'Z'] = chr $ 65 + (ord char - 65 + places) `mod` 26
          | char `elem` ['A'..'Z'] = chr $ 97 + (ord char - 97 + places) `mod` 26
          where places = ord key - 65
        shift _ _ = ' '
        zipWithoutSpaces [] _ = []
        zipWithoutSpaces _ [] = []
        zipWithoutSpaces (' ':xs) y = (' ', ' ') : zipWithoutSpaces xs y
        zipWithoutSpaces (x:xs) (y:ys) = (x, y) : zipWithoutSpaces xs ys
