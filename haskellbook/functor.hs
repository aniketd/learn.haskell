{-# LANGUAGE InstanceSigs #-}
-- starting with "QuickChecking the Functor instances"

-- fmap id      = id
-- fmap (p . q) = (fmap p) . (fmap q)

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f)  x


-- exercises

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap :: (a -> b) -> Possibly a -> Possibly b
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap :: (c -> b) -> Sum a c -> Sum a b
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)
