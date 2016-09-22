module NotThe where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe "The" = Nothing
notThe x     = Just x

replaceThe :: String -> String
replaceThe = unwords . fmap trans . words
  where trans word = trans' (notThe word)
        trans' Nothing = "a"
        trans' (Just x)  = x

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = foldCount 0 . words
  where foldCount acc []  = acc
        foldCount acc [_] = acc
        foldCount acc (the:x:xs)
          | (the == "the" || the == "The") && head x `elem` ['a','e','i','o','u','A','E','I','O','U'] = foldCount (acc + 1) xs
          | otherwise                                                                                 = foldCount acc (x:xs)

isVowel :: Char -> Bool
isVowel c
  | c `elem` ['a','e','i','o','u','A','E','I','O','U'] = True
  | otherwise                                          = False

filterVowels :: String -> String
filterVowels = filter isVowel

filterConsos :: String -> String
filterConsos = filter (not . isVowel)

countVowels :: String -> Int
countVowels = length . filterVowels

countConsos :: String -> Int
countConsos = length . filterConsos

newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = if vowelsVsConsos s ==  GT
              then Nothing
              else Just $ Word' s
           where vowelsVsConsos t = compare (countVowels t) (countConsos t)
