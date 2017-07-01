import Lib
import Test.QuickCheck
import Test.QuickCheck.Function

type IdentityLaw f a = f a -> Bool

functorIdentity :: (Functor f, Eq (f a)) => IdentityLaw f a
functorIdentity f = fmap id f == f

type ComposeLaw f a b c = f a -> Fun a b -> Fun b c -> Bool

functorCompose :: (Eq (f c), Functor f) => ComposeLaw f a b c
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

main :: IO ()
main = do
  quickCheck (functorIdentity :: IdentityLaw Identity Int)
  quickCheck (functorCompose :: ComposeLaw Identity Int String Char)

  quickCheck (functorIdentity :: IdentityLaw Pair Int)
  quickCheck (functorCompose :: ComposeLaw Pair Int String Char)

  quickCheck (functorIdentity :: IdentityLaw (Two Int) Int)
  quickCheck (functorCompose :: ComposeLaw (Two Int) Int String Char)

  quickCheck (functorIdentity :: IdentityLaw (Three Int String) Int)
  quickCheck (functorCompose :: ComposeLaw (Three Int String) Int String Char)

  quickCheck (functorIdentity :: IdentityLaw (Three' Int) Int)
  quickCheck (functorCompose :: ComposeLaw (Three' Int) Int String Char)

  quickCheck (functorIdentity :: IdentityLaw (Four Int Int Int) Int)
  quickCheck (functorCompose :: ComposeLaw (Four Int Int Int) Int String Char)

  quickCheck (functorIdentity :: IdentityLaw (Four' Int) Int)
  quickCheck (functorCompose :: ComposeLaw (Four' Int) Int String Char)

