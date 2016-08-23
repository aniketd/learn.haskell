module TypeCheck where

-- data Mood = Blah
--           | Woot deriving Show
-- 
-- settleDown x = if x == Woot
--                     then Blah
--                     else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "julie" "loves" "dogs"


data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)
