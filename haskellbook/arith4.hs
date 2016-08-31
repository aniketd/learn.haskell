module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = (read :: a -> Int) . show

main = do
    print (roundTrip 4) 
    print (id 4)
