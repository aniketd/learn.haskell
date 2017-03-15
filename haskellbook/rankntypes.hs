{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a. f a -> g a
