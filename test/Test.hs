{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, DeriveGeneric #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.SmallCheck.Series as SC
import GHC.Generics

import Control.Monad (liftM3)
import Data.Functor

import Data.List
import Data.Ord

import Data.CDAR

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [dyadic
  ,approx
  ,creal
  ,unitTests]

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
propertiesD = testGroup "Properties of Dyadic" [scPropD, qcPropD]

scPropD = testGroup "(checked by SmallCheck)"
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

qcPropD = testGroup "(checked by QuickCheck)"
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
approx = testGroup "Approx" [scPropA, qcPropA]

instance Monad m => Serial m Approx where
  series = cons0 Bottom \/ (tripleToApprox <$> series)
    where
      tripleToApprox (m, SC.NonNegative e, s) = Approx m e s

instance Arbitrary Approx where
    arbitrary = frequency [(10,
                            liftM3 Approx
                                   arbitrary 
                                   (return 0)
                                   (choose (-100,20))),
                           (55,
                            liftM3 Approx
                                   arbitrary 
                                   (elements [1,2,3,254,255,256,257,510,511,512,513,767,768,1020,1021,1022,1023,1024,1025])

                                   (choose (-100,20))),
                           (30,
                            liftM3 Approx
                                   arbitrary 
                                   (choose (0,1025))
                                   (choose (-100,20))),
                           (5, return Bottom)]

scPropA :: TestTree
scPropA = testGroup "(checked by smallcheck)"
  [ SC.testProperty "read . show = id" $ \a -> (a :: Approx) == read (show a)
  , SC.testProperty "fromEDI . toEDI = id" $ \a -> (a :: Approx) == fromEDI (toEDI a)
  , SC.testProperty "diameter" $ \a -> diameter a == upperBound a - lowerBound a
  , SC.testProperty "fromDyadic exact and centre" $ \d -> let a = fromDyadic d in exact a && Just d == centre a
  , SC.testProperty "multiplication" $ \a b -> (toEDI (a*b) == (toEDI a) * (toEDI b))
  , SC.testProperty "addition" $ \a b -> (toEDI (a+b) == (toEDI a) + (toEDI b))
  , SC.testProperty "abs negate sign" $ \a -> approximatedBy 0 $ (a ::Approx) + (negate (signum a) * (abs a))
  , SC.testProperty "a+(-a) contains 0" $ \a -> 0 `approximatedBy` ((a :: Approx) + negate a)
  , SC.testProperty "toApprox r contains r" $ \r -> approximatedBy r $ toApprox 20 (r :: Rational)
  , SC.testProperty "1/(1/a) contains a" $ \a -> not (0 `approximatedBy` a) SC.==> 
                                                       a `better` (1/(1/a))
  , SC.testProperty "a*(1/a) contains 1" $ \a -> not (0 `approximatedBy` a) SC.==> 1 `approximatedBy` (a*(1/a))
  , SC.testProperty "boundErrorTerm" $
    \a -> case a of
            Approx _ _ _ -> let b@(Approx _ e _) = boundErrorTerm a
                            in (a `better` b) && (e < 1024) -- 1024 is errorBound when errorBits is 10
            Bottom       -> better a (boundErrorTerm a)
  , SC.testProperty "limitSize" $
    \a -> case a of
            Approx _ _ _ -> let b@(Approx _ _ s) = limitSize 2 a
                            in (a `better` b) && (s >= -2)
            Bottom       -> better a (limitSize 2 a)
  , SC.testProperty "sqrt" $ \a -> let b = abs a in better b $ (sqrtA 0 b)^2
  ]

qcPropA :: TestTree
qcPropA = testGroup "(checked by quickcheck)"
  [ QC.testProperty "read . show = id" $ \a -> (a :: Approx) == read (show a)
  , QC.testProperty "fromEDI . toEDI = id" $ \a -> (a :: Approx) == fromEDI (toEDI a)
  , QC.testProperty "diameter" $ \a -> diameter a == upperBound a - lowerBound a
  , QC.testProperty "fromDyadic exact and centre" $ \d -> let a = fromDyadic d in exact a && Just d == centre a
  , QC.testProperty "multiplication" $ \a b -> (toEDI (a*b) == (toEDI a) * (toEDI b))
  , QC.testProperty "addition" $ \a b -> (toEDI (a+b) == (toEDI a) + (toEDI b))
  , QC.testProperty "abs negate sign" $ \a -> approximatedBy 0 $ (a ::Approx) + (negate (signum a) * (abs a))
  , QC.testProperty "addition precision" $ \a b -> 
      collect (precision (boundErrorTerm (a+b)) - (precision a `min` precision b))
              True
{-
  , QC.testProperty "multiplication precision" $ \a b -> 
      collect (precision (boundErrorTerm (a*b)) - (precision a `min` precision b))
              True
-}
  , QC.testProperty "a+(-a) contains 0" $ \a -> 0 `approximatedBy` ((a :: Approx) + negate a)
  , QC.testProperty "toApprox r contains r" $ \r -> approximatedBy r $ toApprox 20 (r :: Rational)
  , QC.testProperty "1/(1/a) contains a" $ \a -> not (0 `approximatedBy` a) QC.==> 
                                                       a `better` (1/(1/a))
  , QC.testProperty "a*(1/a) contains 1" $ \a -> not (0 `approximatedBy` a) QC.==> 1 `approximatedBy` (a*(1/a))
  , QC.testProperty "boundErrorTerm" $
    \a -> case a of 
            Approx _ _ _ -> let b@(Approx _ e _) = boundErrorTerm a
                            in (a `better` b) && (e < 1024) -- 1024 is errorBound when errorBits is 10
            Bottom       -> better a (boundErrorTerm a)
  , QC.testProperty "limitSize" $
    \a -> case a of 
            Approx _ _ _ -> let b@(Approx _ _ s) = limitSize 2 a
                            in (a `better` b) && (s >= -2)
            Bottom       -> better a (limitSize 2 a)
  , QC.testProperty "sqrt" $ \a -> let b = abs a in better b $ (sqrtA 0 b)^2
  ]

creal :: TestTree
creal = testGroup "CReal" [propertiesCR]

propertiesCR :: TestTree
propertiesCR = testGroup "Properties of CReal" [scPropCR, qcPropCR]

scPropCR = testGroup "(checked by smallCheck)"
  []

qcPropCR = testGroup "(checked by quickCheck)"
  [
--    QC.testProperty "trigonometric identity" $ \x -> let y = fromDouble x
--                                                     in 1 `approximatedBy` (require 40 $ (sin y)^2 + (cos y)^2)
--  , QC.testProperty "trigonometric identity 1000" $ \x -> let y = fromDoubleAsExactValue x
--                                                          in 1 `approximatedBy` (require 1000 $ (sin y)^2 + (cos y)^2)
  ]

testShowA :: Approx -> String
testShowA a = let (Finite l) = lowerBound a
                  (Finite u) = upperBound a
              in showA (fromDyadic l) ++ "\n" ++
                 showA a ++ "\n" ++
                 showA (fromDyadic u) ++ "\n"

unitTests = testGroup "Unit tests"
  [ testCase "showA 1" $ showA (Approx 7 2 2) @?= "3~"
  , testCase "showA 2" $ showA (Approx 7 2 3) @?= "~~"
  , testCase "showA (Approx 99 2 0)" $ showA (Approx 99 2 0) @?= "10~"
  ]
