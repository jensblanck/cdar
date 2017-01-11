{-# LANGUAGE BangPatterns,GADTs,TypeSynonymInstances,FlexibleInstances #-}
{-|
An implementation of computable real arithmetic using lists of rapidly shrining centred dyadic intervals.
-}
module Data.CDAR.BR where

import           Control.Applicative (ZipList (..))
import qualified Data.CDAR.Approx as A
import           Data.CDAR.Approx hiding (toDouble,approximatedBy)
import           Data.CDAR.IntegerLog
import           Data.CDAR.POrd
import           Data.List

-- | The data type of computable reals as ZipLists of Approx.
-- The ZipList construction allows us to use applicative style.
type CReal = ZipList Approx
--newtype BR a = BR {getBR :: [a]}

type Resources = Int

startLimit :: Int
startLimit = 80

bumpLimit :: Int -> Int
bumpLimit n = n * 3 `div` 2

resources :: ZipList Resources
resources = ZipList $ iterate bumpLimit startLimit

-- instance Functor ZipList where
--     fmap f = ZipList . map f . getZipList

-- instance Applicative ZipList where
--     pure = ZipList . repeat
--     (ZipList f) <*> (ZipList x) = ZipList $ zipWith ($) f x

-- instance Eq a => Eq (ZipList a) where
--     _ == _ = False

instance Show CReal where
    show = show . require 40

instance Num CReal where
    x + y = (\a b l -> ok 10 $ limitAndBound l (a + b)) <$> x <*> y <*> resources
    x * y = (\a b l -> ok 10 $ limitAndBound l (a * b)) <$> x <*> y <*> resources
    negate x = negate <$> x
    abs x = abs <$> x
    signum x = signum <$> x
    fromInteger n = pure (Approx n 0 0)

instance Fractional CReal where
    recip x = recipA <$> resources <*> x
    fromRational x = toApprox <$> resources <*> pure x

-- instance (Ord a) => Ord (ZipList a) where
--     compare = undefined

instance Real CReal where
    toRational = toRational . require 40

showCRealN :: Int -> ZipList Approx -> String
showCRealN n = concat . intersperse "\n" . map showA . take n . getZipList

ok :: Int -> Approx -> Approx
ok d a = if precision a > fromIntegral d then a else Bottom

require :: Int -> ZipList Approx -> Approx
require d x = head . dropWhile (== Bottom) . getZipList $ ok d <$> x

toDouble :: ZipList Approx -> Double
toDouble = A.toDouble . require (54+errorBits)

transposeZipList :: [ZipList a] -> ZipList [a]
transposeZipList = ZipList . transpose . map getZipList

polynomial :: [ZipList Approx] -> ZipList Approx -> ZipList Approx
polynomial as x = 
    (\as' x' l -> ok 10 . limitAndBound l $ poly as' x') <$> transposeZipList as <*> x <*> resources

taylor :: [ZipList Approx] -> ZipList Approx -> ZipList Approx
taylor as x =
    (\as' x' l -> sum . takeWhile nonZeroCentred . map (limitAndBound l) $ zipWith (*) as' (pow x'))
    <$> transposeZipList as <*> x <*> resources

epsilon :: ZipList Approx
epsilon = Approx 0 1 . negate <$> resources

sqrtCR :: ZipList Approx -> ZipList Approx
sqrtCR x = (\a l -> ok 10 . limitAndBound l $ sqrtA (-l) a) <$> x <*> resources

fac :: [ZipList Approx]
fac = map fromInteger $ 1 : scanl1 (*) [1..]

oddFac :: [ZipList Approx]
oddFac = let f (_:x:xs) = x:f xs
             f _ = error "Impossible"
         in f fac

evenFac :: [ZipList Approx]
evenFac = let f (x:_:xs) = x:f xs
              f _ = error "Impossible"
          in f fac

alternateSign :: Num a => [a] -> [a]
alternateSign = zipWith (*) (cycle [1,-1])

atanCR :: ZipList Approx -> ZipList Approx
atanCR x =
  let rr y = y / (1 + sqrt (1 + x^2))
      x' = rr . rr . rr $ x -- range reduction so that |a'| < 1/4
      x2 = - x'^2
      t = epsilon + x * taylor (map ((1/) . fromIntegral) [1,3..]) x2
  in boundErrorTerm . (8*) <$> t
--  let x2 = x^2
--           in epsilon + x * taylor (map (1/) . alternateSign . map fromInteger $ [1,3..]) x2

piCRMachin :: ZipList Approx
piCRMachin = 4*(4*atanCR (1/5)-atanCR (1/239))

piMachinCR :: ZipList Approx
piMachinCR = piMachinA . negate <$> resources

piBorweinCR :: ZipList Approx
piBorweinCR = piBorweinA . negate <$> resources

piBinSplitCR :: ZipList Approx
piBinSplitCR = limitAndBound <$> resources <*> (require <$> resources <*> ZipList (repeat (ZipList piRaw)))

ln2 :: ZipList Approx
ln2 = log2A . negate <$> resources

expCR :: ZipList Approx -> ZipList Approx
expCR = (+ epsilon) . taylor (map (1/) $ fac)

halfPi :: ZipList Approx
halfPi = pure (Approx 1 0 (-1)) * pi

sinRangeReduction :: ZipList Approx -> ZipList Approx
sinRangeReduction x = (subtract halfPi) . abs . (pi -) . abs . (subtract halfPi) $ modA <$> x <*> 2 * pi

sinRangeReduction2 :: ZipList Approx -> ZipList Approx
sinRangeReduction2 x = let k = (\a -> case a of 
                                        (Approx m _ s) -> max 0 $ 8 * (integerLog2 m + s + 3) `div` 5
                                        Bottom -> 0) <$> x
                           y = sinCRTaylor (x / (fromIntegral . (3^) <$> k))
                           step z = z*(3-4*z^2)
                       in (\y' k' l -> limitAndBound l $ foldr ($) y' (replicate k' step)) <$> y <*> k <*> resources

sinCRTaylor :: ZipList Approx -> ZipList Approx
sinCRTaylor x = let x2 = x^2
                in epsilon + x * taylor (map (1/) $ alternateSign oddFac) x2

sinCR :: ZipList Approx -> ZipList Approx
sinCR = sinRangeReduction2 . sinRangeReduction

cosCR :: ZipList Approx -> ZipList Approx
cosCR = sinCR . (halfPi -)

instance Floating CReal where
    sqrt x = sqrtA <$> resources <*> x
    pi = piBinSplitCR
    exp x = expA <$> resources <*> x
    log x = logA <$> resources <*> x -- logAgmA is an alternative
    sin x = sinA <$> resources <*> x
    cos x = cosA <$> resources <*> x
    asin = undefined
    atan x = atanA <$> resources <*> x
    acos = undefined
    sinh = undefined
    cosh = undefined
    asinh = undefined
    atanh = undefined
    acosh = undefined

instance PartialOrd CReal where
    partialCompare a b = head . dropWhile (== Nothing) . getZipList $ partialCompare <$> a <*> b
