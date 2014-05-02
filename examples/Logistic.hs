{-# LANGUAGE NoMonomorphismRestriction #-}
module Logistic where

import System.Environment
import Data.CDAR.Approx hiding (toDouble)
import Data.CDAR.BR
import Data.Ratio
import Text.Printf

logMap :: (Num a) => a -> a
logMap x = 4*x*(1-x)

orbit :: Fractional a => [a]
orbit = iterate logMap (fromRational (1%8))

test k = mapM_ putStr .
         map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
         take k $ zip3 [0..] (orbit :: [Double]) (orbit :: [BR Approx])

logMapC :: (Fractional a) => Rational -> a -> a
logMapC c x = (fromRational c)*x*(1-x)

orbitC :: Fractional a => Rational -> [a]
orbitC c = iterate (logMapC c) (fromRational (1%8))

testC c k = mapM_ putStr .
            map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
            take k $ zip3 [0..] (orbitC c:: [Double]) (orbitC c:: [BR Approx])
