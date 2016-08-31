module MyWords where

myWords :: Char -> String -> [String]
myWords c s = go [] s c
    where go acc "" c       = reverse acc
          go acc (x:ss) c
                | x == c    = go acc ss c
                | otherwise = go ((takeWhile (/= c) (x:ss)) : acc) (dropWhile (/= c) (x:ss)) c

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
        ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = myWords '\n'

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

main :: IO ()
main =
    print $ "Are they equal? "
        ++ show (myLines sentences == shouldEqual)
