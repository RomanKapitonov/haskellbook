module BiFunctor where

class Bifunctor p where
  {-# minimal bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b) = Deux (f a) b
  second g (Deux a b) = Deux a (g b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  first f (Const a) = Const (f a)
  second _ (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c) = Drei a (f b) c
  second g (Drei a b c)= Drei a b (g c)

data Superdrei a b c = Superdrei a b

instance Bifunctor (Superdrei a) where
  bimap f g (Superdrei a b) = Superdrei a (f b)
  first f (Superdrei a b) = Superdrei a (f b)
  second g (Superdrei a b) = Superdrei a b

data Quadriceps a b c d = Quadriceps a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (g d)
  first f (Quadriceps a b c d) = Quadriceps a b (f c) d
  second g (Quadriceps a b c d) = Quadriceps a b c (g d)

data Meither a b = MLeft a | MRight b

instance Bifunctor Meither where
  bimap f _ (MLeft a) = MLeft (f a)
  bimap _ g (MRight b) = MRight (g b)
  first f (MLeft a) = MLeft (f a)
  first _ (MRight b) = MRight b
  second _ (MLeft a) = MLeft a
  second g (MRight b) = MRight (g b)
