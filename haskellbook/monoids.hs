module Monoids where

import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend Nada (Only x) = Only x
  mappend (Only x) Nada = Only x
  mappend Nada Nada = Nada


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin e a n ad =
  mconcat [e, "! he said ", a, " as he jumped into his car ", n, " and drove off with his ", ad, " wife."]

newtype First' a =
  First' { getFirst :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) _ = First' (Only x)
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend _ _ = First' Nada
