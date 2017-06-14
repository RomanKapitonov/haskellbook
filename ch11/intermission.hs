1. Type constructor
2. * -> *
3. *
4. Num a => Doggies a
5. Doggies Integer
6. Doggies [Char]
7. Both
8. doge -> DogueDeBordeaux doge
9. DogueDeBordeaux [Char]

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)