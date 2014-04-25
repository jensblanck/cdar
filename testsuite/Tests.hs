{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test
--import Data.CDAR.Internal
import qualified Data.CDAR.Dyadic as D
import Data.CDAR.Dyadic hiding (normalise)
import qualified Data.CDAR.Interval
import Data.CDAR.Approx
import Data.CDAR.BR
import Control.Monad
import Text.Printf

longArgs = stdArgs {maxSuccess=500}

-- Properties to check for Dyadic numbers

instance Arbitrary Dyadic where
    arbitrary = do a <- arbitrary
		   s <- choose (-1000,100)
		   return (a :^ s)
--    coarbitrary (a :^ _) = variant (fromInteger a `mod` 4)

prop_Ord :: Dyadic -> Dyadic -> Bool
prop_Ord a b = (compare a b) == (compare (toRational a) (toRational b))

prop_DyadicMul :: Dyadic -> Dyadic -> Bool
prop_DyadicMul a b = toRational (a*b) == (toRational a)*(toRational b)

prop_DyadicAdd :: Dyadic -> Dyadic -> Bool
prop_DyadicAdd a b = toRational (a+b) == (toRational a)+(toRational b)

prop_DyadicNeg :: Dyadic -> Bool
prop_DyadicNeg a = a + negate a == 0

prop_DyadicNormalise :: Dyadic -> Bool
prop_DyadicNormalise a = a == D.normalise a

prop_absnegsignD :: Dyadic -> Bool
prop_absnegsignD a = a + (negate (signum a) * (abs a)) == 0:^0

prop_readshowD :: Dyadic -> Bool
prop_readshowD a = a == read (show a)

-- Properties of Approx

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

prop_readshowA :: Dyadic -> Bool
prop_readshowA a = a == read (show a)

prop_normalise :: Approx -> Bool
prop_normalise a = a == normalise a

prop_ToFromEDI :: Approx -> Bool
prop_ToFromEDI a = a == (fromEDI . toEDI) a

prop_diameter :: Approx -> Bool
prop_diameter a = diameter a == upperBound a - lowerBound a

prop_fromDyadic :: Dyadic -> Bool
prop_fromDyadic d = let a = fromDyadic d in exact a && d == centre a

prop_Mul :: Approx -> Approx -> Bool
prop_Mul a b =
    (toEDI (a*b) == (toEDI a) * (toEDI b))

prop_Add :: Approx -> Approx -> Bool
prop_Add a b =
    (toEDI (a+b) == (toEDI a) + (toEDI b))

prop_absnegsignA :: Approx -> Bool
prop_absnegsignA a = approximatedBy 0 $ a + (negate (signum a) * (abs a))

prop_Add_precision :: Approx -> Approx -> Property
prop_Add_precision a b =
    collect (precision (boundErrorTerm (a+b)) - (precision a `min` precision b))
	    True

prop_Mul_significance :: Approx -> Approx -> Property
prop_Mul_significance a b =
    collect (significance (boundErrorTerm (a*b)) -
	     (significance a `min` significance b)) True

prop_negA :: Approx -> Bool
prop_negA a = 0 `approximatedBy` (a + negate a)

prop_toApprox :: Rational -> Bool
prop_toApprox r = approximatedBy r $ toApprox 20 r

prop_recRec :: Approx -> Bool --Property
prop_recRec a = --not (0 `approximatedBy` a) ==> 
     a `better` (1/(1/a))

prop_recMul :: Approx -> Property
prop_recMul a = not (0 `approximatedBy` a) ==> 1 `approximatedBy` (a*(1/a))

prop_errorBound :: Approx -> Bool
prop_errorBound a@(Approx _ _ _) = let b@(Approx _ e _) = boundErrorTerm a
		                   in (a `better` b) && (e < errorBound)
prop_errorBound a@Bottom = better a $ boundErrorTerm a

prop_errorBound' :: Approx -> Property
prop_errorBound' a = collect (a, b) ((a `better` b) && (e < errorBound))
    where b@(Approx _ e _) = boundErrorTerm a

prop_limitSize :: Approx -> Bool
prop_limitSize a@(Approx _ _ _) = let b@(Approx _ _ s) = limitSize 2 a
		                  in (a `better` b) && (s >= -2)
prop_limitSize a@Bottom = better a $ limitSize 2 a

prop_sqrtA :: Approx -> Bool
prop_sqrtA a = let b = abs a in better b $ (sqrtA 0 b)^2

-- Properties of BR

instance Arbitrary BR where
    arbitrary = frequency [(50,
                            liftM fromRational arbitrary),
                           (50,
                            liftM fromInteger arbitrary)]

prop_absnegsignBR :: BR -> Bool
prop_absnegsignBR a = approximatedBy 0 . require 64 $ a + (negate (signum a) * (abs a))

{-
prop_negBR :: BR -> Bool
prop_negBR a = 0 `approximatedBy` (a + negate a)

prop_recMulBR :: BR -> Property
prop_recMulBR a = not (0 `approximatedBy` a) ==> 1 `approximatedBy` (a*(1/a))
-}

-- The tests.

tests = [("-- Dyadic", putStrLn "")
        ,("ordered", quickCheckWith stdArgs prop_Ord)
        ,("mul", quickCheckWith stdArgs prop_DyadicMul)
        ,("add", quickCheckWith stdArgs prop_DyadicAdd)
        ,("negate", quickCheckWith stdArgs prop_DyadicNeg)
        ,("normalise", quickCheckWith stdArgs prop_DyadicNormalise)
        ,("a+(-signum(a))*abs(a) = 0", quickCheckWith stdArgs prop_absnegsignD)
        ,("read . show = id (D)", quickCheckWith stdArgs prop_readshowD)
        ,("-- Approx", putStrLn "")
        ,("valid", quickCheckWith stdArgs valid)
        ,("read . show = id (A)", quickCheckWith stdArgs prop_readshowD)
        ,("normalise", quickCheckWith stdArgs prop_normalise)
        ,("ToFromEDI", quickCheckWith stdArgs prop_ToFromEDI)
        ,("diameter", quickCheckWith stdArgs prop_diameter)
        ,("fromDyadic", quickCheckWith stdArgs prop_fromDyadic)
        ,("mul", quickCheckWith stdArgs prop_Mul)
        ,("add", quickCheckWith stdArgs prop_Add)
        ,("a+(-signum(a))*abs(a) = 0", quickCheckWith stdArgs prop_absnegsignA)
        ,("add precision", quickCheckWith longArgs prop_Add_precision)
        ,("mul significance", quickCheckWith stdArgs prop_Mul_significance)
        ,("negA", quickCheckWith stdArgs prop_negA)
        ,("toApprox", quickCheckWith stdArgs prop_toApprox)
        ,("recip.recip", quickCheckWith stdArgs prop_recRec)
        ,("mul.recip", quickCheckWith stdArgs prop_recMul)
        ,("errorBound", quickCheckWith stdArgs prop_errorBound)
        ,("limitSize", quickCheckWith stdArgs prop_limitSize)
        ,("sqrtA", quickCheckWith stdArgs prop_sqrtA)
        ,("-- BR", putStrLn "")
        ,("a+(-signum(a))*abs(a) = 0", quickCheckWith stdArgs prop_absnegsignBR)]

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
