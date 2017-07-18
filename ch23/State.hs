module State where

newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \s -> (s, s)

-- Prelude> runState get "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- Prelude> runState (put "blah") "woot"
-- ((),"blah")

exec :: State s a -> s -> s
exec (State sa) = \s -> ((\(_, s') -> s') (sa s) )

exec' :: State s a -> s -> s
exec' = (snd .) . runState

-- Prelude> exec (put "wilma") "daphne"
-- "wilma"
-- Prelude> exec get "scooby papu"
-- "scooby papu"

eval :: State s a -> s -> a
eval (State sa) = \s -> ( (\(a, _) -> a) (sa s) )

eval' :: State s a -> s -> a
eval' = (fst .) . runState

-- Prelude> eval get "bunnicula"
-- "bunnicula"
-- Prelude> eval get "stake a bunny"
-- "stake a bunny"

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s
                                   in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State g) = State $ \s -> let (f', s') = f s
                                              (a, s'') = g s'
                                          in  (f' a, s'')

instance Monad (State s) where
  return = pure
  (State f) >>= g = State $ \s -> let (a, s') = f s
                                      ns = runState $ g a
                                  in  ns s'

modify :: (s -> s) -> State s ()
modify f = f <$> get >>= put

-- Prelude> runState (modify (+1)) 0
-- ((),1)
-- Prelude> runState (modify (+1) >> modify (+1)) 0
-- ((),2)