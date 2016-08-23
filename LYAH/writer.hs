import Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang n = (n > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a, String) -> (a -> (b, String)) -> (b, String)
