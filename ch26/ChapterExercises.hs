module ChapterExercises where

import Control.Monad.Trans.Reader
import Data.Functor.Identity

data Hole = Hole
hole = undefined

rDec :: Num a => Reader a a
rDec = ReaderT $ \a -> pure (a - 1)

rDec' :: Num a => Reader a a
rDec' = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> pure (show a)

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ Identity . show
