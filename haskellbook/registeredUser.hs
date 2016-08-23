module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum))
        = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
      Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

-- is it South Africa? If so, return True
isSouthAfrica SouthAfrica = True
isSouthAfrica           _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

-- funcC x y = if (x > y) then x else y
funcC x y =
    case (x > y) of
        True -> x
        False -> y

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.59 = 'D'
    | y <  0.59 = 'F'
    where y = x / 100
