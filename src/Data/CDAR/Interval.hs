-- |Build intervals over arbitrary type.
--
-- Currently allows any type, but it does not make sense for types without
-- 'Ord' instance. The intention is clearly that the type should be a 'Num'
-- type.
module Data.CDAR.Interval (module Data.CDAR.Interval) where

import Data.CDAR.Dyadic
import Data.CDAR.POrd
import Data.Ratio

-- class of averageable numbers, includes fractions and dyadic numbers

-- |A number class is averageable if the midpoint can be exactly represented.
class Num a => Averageable a where
    average :: a -> a -> a

-- | The average of two dyadic numbers is a dyadic number.
instance Averageable Dyadic where
    average x y = let (a:^s) = x+y in a:^(s-1)

-- | Any rational data type is averageable.
instance Integral a => Averageable (Ratio a) where
    average x y = (x+y)/2

-- Intervals

-- | Given a type 'a' this gives a data type of 'a' intervals.
data Interval a = Interval a a deriving (Eq,Read,Show)

-- | Extract the lower bound from an interval.
lowerBound :: Interval a -> a
lowerBound (Interval l _) = l

-- | Extract the upper bound from an interval.
upperBound :: Interval a -> a
upperBound (Interval _ u) = u

-- | Extract the centre from an interval if the underlying data type is averageable.
centre :: Averageable a => Interval a -> a
centre d = average (upperBound d) (lowerBound d)

-- | Computes the diameter of an interval if the underlying type is numeric.
diameter :: Num a => Interval a -> a
diameter d = (upperBound d) - (lowerBound d)

-- | Returns true if interval is a singleton point.
exact :: Eq a => Interval a -> Bool
exact d = upperBound d == lowerBound d

-- | Checks if the first argument is included in the interval.
--
-- Types may be generalised is various ways.
approximatedBy :: (Real a) => a -> Interval a -> Bool
r `approximatedBy` d = toRational (lowerBound d) <= toRational r &&
                       toRational r <= toRational (upperBound d)

-- | Introduces a partial ordering on intervals if the first interval is
-- included in the second.
better :: Ord a => Interval a -> Interval a -> Bool
d `better` e = lowerBound d >= lowerBound e &&
               upperBound d <= upperBound e

-- | Intervals over numeric types are also numeric.
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

-- | If the underlying data type is ordered then intervals are in 'IntervalOrd'.
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

-- | Intervals with dyadic end-points.
type DyadicInterval = Interval Dyadic

-- | The functor instance of intervals maps a function to both of its
-- end-points.
instance Functor Interval where
    fmap f (Interval l u) = Interval (f l) (f u)
