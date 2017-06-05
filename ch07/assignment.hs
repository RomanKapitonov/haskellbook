tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10


foldBool :: a -> a -> Bool -> a
foldBool = error "Error: Need to implement foldBool!"

foldBool' x y b = case b of
  True -> x
  False -> y

foldBool'' x y b
  | b = x
  | otherwise = y


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)