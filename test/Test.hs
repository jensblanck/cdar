{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.SmallCheck.Series

import Data.List
import Data.Ord

import Data.CDAR

instance (Monad m) => Serial m Dyadic where
    series = cons2 (:^)
instance Arbitrary Dyadic where
    arbitrary = do a <- arbitrary
		   s <- choose (-1000,100)
		   return (a :^ s)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "compare dyadic the same as comparing rational" $
      \x y -> compare (x :: Dyadic) (y :: Dyadic) == (compare (toRational x) (toRational y))
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "compare dyadic the same as comparing rational" $ 
      \x y -> compare (x :: Dyadic) (y :: Dyadic) == (compare (toRational x) (toRational y))
  ]

unitTests = testGroup "Unit tests"
  [ testCase "showA" $ showA (Approx 7 2 2) @?= "~~"
  ]
