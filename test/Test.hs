{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, DeriveGeneric #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.SmallCheck.Series as SC
import GHC.Generics

import Data.Functor

import Data.List
import Data.Ord

import Data.CDAR

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dyadic, approx, unitTests]

idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent f = \x -> f(x) == f(f(x))

autoinverse :: Eq a => (a -> a) -> a -> Bool
autoinverse f = \x -> x == f(f(x))

dyadic :: TestTree
dyadic = testGroup "Dyadic" [propertiesD]

deriving instance Generic Dyadic
instance (Monad m) => Serial m Dyadic

instance Arbitrary Dyadic where
    arbitrary = do a <- arbitrary
		   s <- choose (-1000,100)
		   return (a :^ s)

propertiesD :: TestTree
propertiesD = testGroup "Properties of Dyadic" [scPropsD, qcPropsD]

scPropsD = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "compare" $
      \x y -> compare (x :: Dyadic) (y :: Dyadic) == (compare (toRational x) (toRational y))
  , SC.testProperty "multiplication" $
      \a b -> toRational (a*b :: Dyadic) == (toRational a)*(toRational b)
  , SC.testProperty "addition" $
      \a b -> toRational (a+b :: Dyadic) == (toRational a)+(toRational b)
  , SC.testProperty "negate" $
      \a -> (a :: Dyadic) + negate a == 0
  , SC.testProperty "negation exists" $
      \a b -> (a :: Dyadic) + b == 0 SC.==> a == negate b
  , SC.testProperty "negation autoinverse" $ autoinverse (negate :: Dyadic -> Dyadic)
  , SC.testProperty "normalise" $
      \a -> (a :: Dyadic) == normalise a
  , SC.testProperty "normalise idempotent" $ idempotent normalise
  , SC.testProperty "abs idempotent" $ idempotent (abs :: Dyadic -> Dyadic)
  , SC.testProperty "signum idempotent" $ idempotent (signum :: Dyadic -> Dyadic)
  , SC.testProperty "signum in {-1,0,1}" $ \x -> signum (x :: Dyadic) `elem` [-1,0,1]
  , SC.testProperty "negate abs signum" $
      \a -> (a :: Dyadic) + (negate (signum a) * (abs a)) == 0
  , SC.testProperty "read show" $
      \a -> (a :: Dyadic) == read (show a)
  , SC.testProperty "shift left identity" $ \d@(a :^ s) (SC.NonNegative n) -> shiftD (s - n) d == d
  , SC.testProperty "sqrt" $ \d@(a :^ s) -> sqrtD (min 0 (2*s)) (d*d) == abs d
  ]

qcPropsD = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "compare" $ 
      \x y -> compare (x :: Dyadic) (y :: Dyadic) == (compare (toRational x) (toRational y))
  , QC.testProperty "multiplication" $
      \a b -> toRational (a*b :: Dyadic) == (toRational a)*(toRational b)
  , QC.testProperty "addition" $
      \a b -> toRational (a+b :: Dyadic) == (toRational a)+(toRational b)
  , QC.testProperty "negate" $
      \a -> (a :: Dyadic) + negate a == 0
  , QC.testProperty "normalise" $
      \a -> (a :: Dyadic) == normalise a
  , QC.testProperty "negate abs signum" $
      \a -> (a :: Dyadic) + (negate (signum a) * (abs a)) == 0:^0
  , QC.testProperty "read show" $
      \a -> (a :: Dyadic) == read (show a)
  , QC.testProperty "negation autoinverse" $ autoinverse (negate :: Dyadic -> Dyadic)
  , QC.testProperty "normalise idempotent" $ idempotent normalise
  , QC.testProperty "abs idempotent" $ idempotent (abs :: Dyadic -> Dyadic)
  , QC.testProperty "signum idempotent" $ idempotent (signum :: Dyadic -> Dyadic)
  , QC.testProperty "signum in {-1,0,1}" $ \x -> signum (x :: Dyadic) `elem` [-1,0,1]
  , QC.testProperty "shift left identity" $ \d@(a :^ s) (QC.NonNegative n) -> shiftD (s - n) d == d
  , QC.testProperty "sqrt" $ \d@(a :^ s) -> sqrtD (min 0 (2*s)) (d*d) == abs d
  ]

approx :: TestTree
approx = testGroup "Approx" [scPropA]

instance Monad m => Serial m Approx where
  series = cons0 Bottom \/ (tripleToApprox <$> series)
    where
      tripleToApprox (m, SC.NonNegative e, s) = Approx m e s

scPropA :: TestTree
scPropA = testGroup "(checked by smallcheck)"
  [ SC.testProperty "Equal" $ \a -> (a :: Approx) == a
  ]

unitTests = testGroup "Unit tests"
  [ testCase "showA 1" $ showA (Approx 7 2 2) @?= "3~"
  , testCase "showA 2" $ showA (Approx 7 2 3) @?= "~~"
  , testCase "showA (Approx 99 2 0)" $ showA (Approx 99 2 0) @?= "10~"
  ]
