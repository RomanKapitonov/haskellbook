module Optional where

import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (x `mappend` y)
  mappend (Only x) (Nada)   = Only x
  mappend (Nada)   (Only x) = Only x
  mappend _        _        = Nada
