module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum


data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

-- is it South Africa? If so, return True
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt :: Penguin
humboldt = Peng SouthAmerica
gentoo :: Penguin
gentoo = Peng Antarctica
macaroni :: Penguin
macaroni = Peng Antarctica
little :: Penguin
little = Peng Australia
galapagos :: Penguin
galapagos = Peng Galapagos