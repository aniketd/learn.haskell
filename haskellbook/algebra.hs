module Algebra where


data Fiction = Fiction deriving (Eq, Show)
data NonFiction = NonFiction deriving (Eq, Show)

data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
              deriving (Eq, Show)

type AuthorName = String

data Author = Author (AuthorName, BookType)

data Authr = Authr AuthorName Fiction
           | AuthorName NonFiction
           deriving (Eq, Show)
