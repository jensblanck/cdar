module Data.CDAR.Classes where

import Data.Bits

class Scalable a where
  scale :: a -> Int -> a

instance Scalable Integer where
  scale x n
    | n >= 0 = unsafeShiftL x n
    | otherwise = unsafeShiftR (x + bit (-n-1)) (-n)
