import Data.Time

data DatabaseItem   = DbString String
                    | DbNumber Integer
                    | DbDate   UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, World!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = let isDate (DbDate _) = True
                   isDate _          = False
                   getUTC (DbDate x) = x
                   getUTC _          = undefined
               in map getUTC . filter isDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = let isNumber (DbNumber _) = True
                     isNumber _            = False
                     getNumber (DbNumber x) = x
                     getNumber _            = undefined
                 in map getNumber . filter isNumber

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = avg . map fromIntegral . filterDbNumber
        where avg x = (sum x) / fromIntegral (length x)


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

scanf :: Integer -> [Integer]
scanf 0 = 1
scanf n 
