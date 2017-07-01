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
  quickCheck (functorIdentity :: IdentityLaw BoolAndSomethingElse Int)
  quickCheck (functorCompose :: ComposeLaw BoolAndSomethingElse Int String Char)

  quickCheck (functorIdentity :: IdentityLaw BoolAndMaybeSomethingElse Int)
  quickCheck (functorCompose :: ComposeLaw BoolAndMaybeSomethingElse Int String Char)
