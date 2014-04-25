module Data.CDAR.Interval (module Data.CDAR.Interval) where

import Data.CDAR.Dyadic
import Data.CDAR.POrd
import Data.Ratio

-- class of averageable numbers, includes fractions and dyadic numbers

class Num a => Averageable a where
    average :: a -> a -> a

instance Averageable Dyadic where
    average x y = let (a:^s) = x+y in a:^(s-1)

instance Integral a => Averageable (Ratio a) where
    average x y = (x+y)/2

-- Intervals

data Interval a = Interval a a deriving (Eq,Read,Show)

lowerBound :: Interval a -> a
lowerBound (Interval l _) = l

upperBound :: Interval a -> a
upperBound (Interval _ u) = u

centre :: Averageable a => Interval a -> a
centre d = average (upperBound d) (lowerBound d)

diameter :: Num a => Interval a -> a
diameter d = (upperBound d) - (lowerBound d)

exact :: Eq a => Interval a -> Bool
exact d = upperBound d == lowerBound d

approximatedBy :: (Real a) => a -> Interval a -> Bool
r `approximatedBy` d = toRational (lowerBound d) <= toRational r &&
		       toRational r <= toRational (upperBound d)

better :: Ord a => Interval a -> Interval a -> Bool
d `better` e = lowerBound d >= lowerBound e &&
	       upperBound d <= upperBound e

instance (Num a, Ord a) => Num (Interval a) where
    (Interval l u) + (Interval l' u') = Interval (l+l') (u+u')
    (Interval l u) * (Interval l' u')
       = let t = [l*l',l*u',u*l',u*u']
           in Interval (minimum t) (maximum t)     -- better algorithms exist
    negate (Interval l u) = Interval (negate u)  (negate l)
    abs i@(Interval l u)
	| (signum l) /= -1 = i
	| (signum u) /= 1  = negate i
	| otherwise        = Interval 0 (negate l `max` u)
    signum (Interval l u)
	| (signum l) == 1  = 1
	| (signum u) == -1 = -1
	| otherwise        = error "signum not defined, interval spans 0"
    fromInteger i = let d = fromInteger i in Interval d d

instance Ord a => IntervalOrd (Interval a) where
    intervalCompare a b = case compare la lb of
                            LT -> if ua < lb
                                  then LessThan
                                  else if ua < ub
                                       then Overlap
                                       else Include
                            EQ -> case compare ua ub of
                                    LT -> Within
                                    EQ -> Same
                                    GT -> Include
                            GT -> if la > ub
                                  then GreaterThan
                                  else if ua > ub
                                       then Overlap
                                       else Within
        where la = lowerBound a
              ua = upperBound a
              lb = lowerBound b
              ub = upperBound b

-- Dyadic intervals (End-point based dyadic intervals)

type DyadicInterval = Interval Dyadic

instance Functor Interval where
    fmap f (Interval l u) = Interval (f l) (f u)
