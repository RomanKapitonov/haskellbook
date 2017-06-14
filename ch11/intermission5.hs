-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show
type Gardener = String

-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

-- Normal Form (Sum of products):

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show