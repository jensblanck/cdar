{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.CDAR
import Data.Ratio()

data Coord i a = Coord { chart :: i
                       , point :: a
                       } deriving (Eq, Ord, Read, Show)

data Charts = North | South deriving (Eq, Ord, Read, Show)

type TL = Coord Charts Rational
type TLCR = Coord Charts CR

toChart :: (Eq i, Fractional a) => i -> Coord i a -> Maybe (Coord i a)
toChart c t@(Coord k x)
  | c == k = Just t
  | otherwise = Just $ Coord c (1/x)

transition :: (Eq i, Fractional a) => i -> i -> Coord i a -> Maybe (Coord i a)
transition k k' (Coord c x)
  | k == c && k /= k' = Just $ Coord k' (1/x)
  | k == c && k == k' = Just $ Coord k x
  | otherwise = Nothing

transition' :: (Eq i, Fractional a) => i -> i -> Coord i a -> Coord i a
transition' k k' (Coord c x)
  | k == c = Coord k' (1/x)
transition' _ _ _ = undefined

showTL :: TL -> String
showTL (Coord North 0) = "∞"
showTL (Coord North x) = show (1/x)
showTL (Coord South x) = show x

instance {-# OVERLAPPABLE #-} (Eq a, Fractional a) => Num (Coord Charts a) where
  (Coord North 0) + (Coord North _) = Coord North 0
  (Coord North _) + (Coord North 0) = Coord North 0
  (Coord North x) + (Coord North y) = Coord North (1/(1/x + 1/y))
  (Coord North 0) + (Coord South _) = Coord North 0
  (Coord North x) + (Coord South y) = Coord South (1/x + y)
  (Coord South _) + (Coord North 0) = Coord North 0
  (Coord South x) + (Coord North y) = Coord South (x + 1/y)
  (Coord South x) + (Coord South y) = Coord South (x+y)
  (Coord North x) * (Coord North y) = Coord North (x*y)
  (Coord North x) * (Coord South y) = Coord South (y/x)
  (Coord South x) * (Coord North y) = Coord South (x/y)
  (Coord South x) * (Coord South y) = Coord South (x*y)
  negate (Coord c x) = Coord c (negate x)
  abs (Coord c x) = Coord c (abs x)
  signum (Coord c x) = Coord c (signum x)
  fromInteger n = Coord South (fromInteger n)

instance (Eq a, Fractional a) => Fractional (Coord Charts a) where
  recip (Coord North x) = Coord South x
  recip (Coord South x) = Coord North x
  fromRational x = Coord South (fromRational x)

instance (Eq a, Fractional a, Ord a, Real a) => Real (Coord Charts a) where
  toRational (Coord South x) = toRational x
  toRational (Coord North x) = 1/toRational x

instance {-# OVERLAPPING #-} Num (Coord Charts Approx) where
  _ + _ = undefined
  _ * _ = undefined
  negate (Coord c x) = Coord c (negate x)
  abs (Coord c x) = Coord c (abs x)
  signum (Coord c x) = Coord c (signum x)
  fromInteger n = Coord South (fromInteger n)

main :: IO ()
main = print "Hej"

