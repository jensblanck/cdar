module Logistic (module Logistic
                ,module Data.CDAR.BR) where

import System.Environment
import Data.CDAR.Approx hiding (toDouble)
import Data.CDAR.BR
import Data.Ratio

logMap :: (Num a) => a -> a
logMap x = 4*x*(1-x)

orbit :: Fractional a => [a]
orbit = iterate logMap (fromRational (1%8))

main = do
  (a:b:_) <- getArgs
  doit a (read b)

doit "BR" n = do
  print $ take n (orbit :: [BR Approx])
doit "BR2d" n = do
  print . map toDouble $ take n (orbit :: [BR Approx])
doit _ n = do
  print $ take n (orbit :: [Double])

