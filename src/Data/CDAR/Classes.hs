{-|
Defines the 'Scalable' type class.
-}
module Data.CDAR.Classes where

import Data.Bits

-- | 'Scalable' allows scaling numerical data types by powers of 2.
class Scalable a where
  scale :: a -> Int -> a

-- | The 'Integer' instance.
instance Scalable Integer where
  scale x n
    | n >= 0 = unsafeShiftL x n
    | otherwise = unsafeShiftR (x + bit (-n-1)) (-n)

type Precision = Int

class ShowApprox a where
  showA :: a -> String
  showInBaseA :: Int -> a -> String

class IntervalOps a where
  lowerA :: a -> a
  upperA :: a -> a
  centreA :: a -> a
  exact :: a -> Bool
  approximatedBy :: Real b => b -> a -> Bool
  better :: a -> a -> Bool

class ToDouble a where
  toDoubleA :: a -> Maybe Double

class ApproxOps a where
  boundErrorTerm :: a -> a
  limitSize :: Precision -> a -> a
  checkPrecisionLeft :: a -> a
  limitAndBound :: Precision -> a -> a
  unionA :: a -> a -> a
  intersectionA :: a -> a -> a
  consistentA :: a -> a -> Bool
  poly :: [a] -> a -> a
  powers :: a -> [a]

-- |Number of bits that error term is allowed to take up. A larger size allows
-- for more precise but slightly more costly computations. The value here is
-- suggested by test runs.
errorBits :: Int
errorBits = 10

errorBound :: Integer
errorBound = 2^errorBits

-- |The default cutoff for diverging computations. May well be chosen much
-- smaller. 31 corresponds to about 10 decimal places.
defaultPrecision :: Precision
defaultPrecision = 31

