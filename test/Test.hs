{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.SmallCheck.Series

import Data.List
import Data.Ord

import Data.CDAR

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dyadic, unitTests]

dyadic :: TestTree
dyadic = testGroup "Dyadic" [propertiesD]

instance (Monad m) => Serial m Dyadic where
    series = cons2 (:^)
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
  , SC.testProperty "negate exists" $
      \a b -> (a :: Dyadic) + b == 0 SC.==> a == negate b
  , SC.testProperty "normalise" $
      \a -> (a :: Dyadic) == normalise a
  , SC.testProperty "negate abs signum" $
      \a -> (a :: Dyadic) + (negate (signum a) * (abs a)) == 0:^0
  , SC.testProperty "read show" $
      \a -> (a :: Dyadic) == read (show a)
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
  ]

unitTests = testGroup "Unit tests"
  [ testCase "showA 1" $ showA (Approx 7 2 2) @?= "3~"
  , testCase "showA 2" $ showA (Approx 7 2 3) @?= "~~"
  , testCase "showA 3" $ showA (Approx 13 13 2) @?= "1~~"
  ]
