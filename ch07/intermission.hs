addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (\n -> n + 1)

-- addFive x y = (if x > y then y else x) + 5
addFive = (\x -> (\y -> if x > y then y + 5 else x + 5)) :: Integer -> Integer -> Integer

mflip f x y = f y x
