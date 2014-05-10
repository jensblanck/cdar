{-# LANGUAGE NoMonomorphismRestriction #-}
module Logistic where

import System.Environment
import Data.CDAR.Approx hiding (toDouble)
import Data.CDAR.BR
import Data.Bits
import Data.Ratio
import Text.Printf

logMap :: (Num a) => a -> a
logMap x = 4*x*(1-x)

logMap2 :: Approx -> Approx
logMap2 Bottom = Bottom
logMap2 (Approx m e s) =
    let one = bit (-s)
        t = abs (bit (-s-1) - m)
    in boundErrorTerm $
       if t >= e
       then Approx (m*(one-m)-e^2) (e * abs (one-2*m)) (2*s)
       else Approx (bit (-2*s - 2)) ((t + e)^2) (2*s)

orbit :: Fractional a => [a]
orbit = iterate logMap (fromRational (1%8))

orbit1 :: [BR Approx]
orbit1 = iterate (polynomial [0,4,-4]) (fromRational (1%8))

orbit2 :: [BR Approx]
orbit2 = iterate ((4*) . fmap logMap2) (fromRational (1%8))

test k = mapM_ putStr .
         map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
         take k $ zip3 [0..] (orbit :: [Double]) (orbit :: [BR Approx])

test1 k = mapM_ putStr .
          map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
          take k $ zip3 [0..] (orbit :: [Double]) (orbit1 :: [BR Approx])

test2 k = mapM_ putStr .
          map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
          take k $ zip3 [0..] (orbit :: [Double]) (orbit2 :: [BR Approx])

logMapC :: (Fractional a) => Rational -> a -> a
logMapC c x = (fromRational c)*x*(1-x)

orbitC :: Fractional a => Rational -> [a]
orbitC c = iterate (logMapC c) (fromRational (1%8))

orbitC1 :: Rational -> [BR Approx]
orbitC1 c = iterate (((fromRational c) *) . polynomial [0,1,-1]) (fromRational (1%8))

testC c k = mapM_ putStr .
            map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
            take k $ zip3 [0..] (orbitC c:: [Double]) (orbitC c:: [BR Approx])
