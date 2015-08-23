{-# LANGUAGE BangPatterns,GADTs,TypeSynonymInstances,FlexibleInstances #-}
module Data.CDAR.BR where

import           Control.Applicative
import qualified Data.CDAR.Approx as A
import           Data.CDAR.Approx hiding (toDouble,approximatedBy)
import           Data.CDAR.IntegerLog
import           Data.CDAR.POrd
import           Data.List
--import Data.Monoid

-- The data type of reals as ZipLists of Approx.
-- The ZipList construction allows us to use applicative style.
newtype BR a = BR {getBR :: [a]}

type Resources = Int

startLimit :: Int
startLimit = 80

bumpLimit :: Int -> Int
bumpLimit n = n * 3 `div` 2

resources :: BR Resources
resources = BR $ iterate bumpLimit startLimit

instance Functor BR where
    fmap f = BR . map f . getBR

instance Applicative BR where
    pure = BR . repeat
    (BR f) <*> (BR x) = BR $ zipWith ($) f x

instance Eq a => Eq (BR a) where
    _ == _ = False

instance Show (BR Approx) where
    show = show . require 40

instance Num (BR Approx) where
    x + y = (\a b l -> ok 10 $ limitAndBound l (a + b)) <$> x <*> y <*> resources
    x * y = (\a b l -> ok 10 $ limitAndBound l (a * b)) <$> x <*> y <*> resources
    negate x = negate <$> x
    abs x = abs <$> x
    signum x = signum <$> x
    fromInteger n = pure (Approx n 0 0)

instance Fractional (BR Approx) where
    recip x = recipA <$> resources <*> x
    fromRational x = toApprox <$> resources <*> pure x

instance (Ord a) => Ord (BR a) where
    compare = undefined

instance Real (BR Approx) where
    toRational = toRational . require 40

showBRn :: Int -> BR Approx -> String
showBRn n = concat . intersperse "\n" . map showA . take n . getBR

ok :: Int -> Approx -> Approx
ok d a = if precision a > fromIntegral d then a else Bottom

require :: Int -> BR Approx -> Approx
require d x = head . dropWhile (== Bottom) . getBR $ ok d <$> x

toDouble :: BR Approx -> Double
toDouble = A.toDouble . require (54+errorBits)

transposeBR :: [BR a] -> BR [a]
transposeBR = BR . transpose . map getBR

polynomial :: [BR Approx] -> BR Approx -> BR Approx
polynomial as x = 
    (\as' x' l -> ok 10 . limitAndBound l $ poly as' x') <$> transposeBR as <*> x <*> resources

taylor :: [BR Approx] -> BR Approx -> BR Approx
taylor as x =
    (\as' x' l -> sum . takeWhile nonZeroCentred . map (limitAndBound l) $ zipWith (*) as' (pow x'))
    <$> transposeBR as <*> x <*> resources

epsilon :: BR Approx
epsilon = Approx 0 1 . negate <$> resources

sqrtBR :: BR Approx -> BR Approx
sqrtBR x = (\a l -> ok 10 . limitAndBound l $ sqrtA (-l) a) <$> x <*> resources

fac :: [BR Approx]
fac = map fromInteger $ 1 : scanl1 (*) [1..]

oddFac :: [BR Approx]
oddFac = let f (_:x:xs) = x:f xs
         in f fac

evenFac :: [BR Approx]
evenFac = let f (x:_:xs) = x:f xs
          in f fac

alternateSign :: Num a => [a] -> [a]
alternateSign = zipWith (*) (cycle [1,-1])

atanBR :: BR Approx -> BR Approx
atanBR x = let x2 = x^2
           in epsilon + x * taylor (map (1/) . alternateSign . map fromInteger $ [1,3..]) x2

piBRMachin :: BR Approx
piBRMachin = 4*(4*atanBR (1/5)-atanBR (1/239))

piMachinBR :: BR Approx
piMachinBR = piMachinA . negate <$> resources

piBorweinBR :: BR Approx
piBorweinBR = piBorweinA . negate <$> resources

piBinSplitBR :: BR Approx
piBinSplitBR = limitAndBound <$> resources <*> (require <$> resources <*> BR (repeat (BR piRaw)))

ln2 :: BR Approx
ln2 = ln2A . negate <$> resources

expBR :: BR Approx -> BR Approx
expBR = (+ epsilon) . taylor (map (1/) $ fac)

halfPi :: BR Approx
halfPi = pure (Approx 1 0 (-1)) * pi

sinRangeReduction :: BR Approx -> BR Approx
sinRangeReduction x = (subtract halfPi) . abs . (pi -) . abs . (subtract halfPi) $ modA <$> x <*> 2 * pi

sinRangeReduction2 :: BR Approx -> BR Approx
sinRangeReduction2 x = let k = (\a -> case a of 
                                        (Approx m _ s) -> max 0 $ 8 * (integerLog2 m + s + 3) `div` 5
                                        Bottom -> 0) <$> x
                           y = sinBRTaylor (x / (fromIntegral . (3^) <$> k))
                           step z = z*(3-4*z^2)
                       in (\y' k' l -> limitAndBound l $ foldr ($) y' (replicate k' step)) <$> y <*> k <*> resources

sinBRTaylor :: BR Approx -> BR Approx
sinBRTaylor x = let x2 = x^2
                in epsilon + x * taylor (map (1/) $ alternateSign oddFac) x2

sinBR :: BR Approx -> BR Approx
sinBR = sinRangeReduction2 . sinRangeReduction

instance Floating (BR Approx) where
    sqrt = sqrtBR
    pi = piBinSplitBR
    exp x = expA <$> x <*> resources
    log x = agmLnA <$> fmap negate resources <*> x
    sin = sinBR
    cos = sin . (halfPi +)
    asin = undefined
    atan x = atanA <$> x <*> resources
    acos = undefined
    sinh = undefined
    cosh = undefined
    asinh = undefined
    atanh = undefined
    acosh = undefined

instance PartialOrd (BR Approx) where
    partialCompare a b = head . dropWhile (== Nothing) . getBR $ partialCompare <$> a <*> b
