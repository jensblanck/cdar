{-# LANGUAGE FlexibleInstances #-}
{-| Intervals do not satisfy the Ord type class. This is a ordering
relation that encodes all ways two intervals may situated in relation to each
other.
-}
module Data.CDAR.POrd where

{- 
Within and Include implies Overlap, but Overlap may be true
regardless. Likewise, LessThan and GreaterThan implies Apart. For normal
intervals Apart is not needed, but if used for interval valued functions, then
it separate.
-}

-- | All possibilies of relations between two intervals.
data IntervalOrdering = Equal | Same | Within | Include | Overlap | LessThan | GreaterThan | Apart
                        deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Interval data types may implement this class to allow that to compare intervals.
class IntervalOrd a where
    intervalCompare :: a -> a -> IntervalOrdering
    equal :: a -> a -> Bool
    same :: a -> a -> Bool
    within :: a -> a -> Bool
    include :: a -> a -> Bool
    overlap :: a -> a -> Bool
    lessThan :: a -> a -> Bool
    greaterThan :: a -> a -> Bool
    apart :: a -> a -> Bool
    a `equal` b = Equal == intervalCompare a b
    a `same` b = Same == intervalCompare a b
    a `within` b = Within >= intervalCompare a b
    a `include` b = let t = intervalCompare a b in t == Same || t == Include
    a `overlap` b = Overlap >= intervalCompare a b
    a `lessThan` b = LessThan == intervalCompare a b
    a `greaterThan` b = GreaterThan == intervalCompare a b
    a `apart` b = LessThan <= intervalCompare a b

-- | This type is a partial version of the usual 'Ordering' type.
type POrdering = Maybe Ordering

-- | Type class with partial comparison operations.
class PartialOrd a where
    partialCompare :: a -> a -> Maybe Ordering
    partialLT :: a -> a -> Maybe Bool
    partialLE :: a -> a -> Maybe Bool
    partialGT :: a -> a -> Maybe Bool
    partialGE :: a -> a -> Maybe Bool
    partialLT a b = fmap (== LT) $ partialCompare a b
    partialLE a b = fmap (not . (== GT)) $ partialCompare a b
    partialGT a b = fmap (== GT) $ partialCompare a b
    partialGE a b = fmap (not . (== LT)) $ partialCompare a b
