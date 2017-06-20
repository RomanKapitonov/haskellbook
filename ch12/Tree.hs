data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f t = case f t of
  Just (l, v, r) -> Node (unfold f l) v (unfold f r)
  Nothing        -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold buildLevel 0
  where
    buildLevel y
      | y < n     = Just (y + 1, y, y + 1)
      | otherwise = Nothing
