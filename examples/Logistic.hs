{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Data.CDAR
import Data.Bits
import Data.Ratio
import Text.Printf

main :: IO ()
main = do
  putStrLn "Showing 60 iterations of the map to show how the Double computation diverges from the CReal computation."
  test2 60
  printf "\nComputing %d iterations of the map.\nIt may take some seconds...\n" n
  printf "\nAfter %d iterations the value is %s\n" n $ showA . limitSize 34 . require 34 $ orbit2!!n
  where n = 50000 :: Int

logMap :: (Fractional a) => a -> a
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

orbit1 :: [CReal]
orbit1 = iterate (polynomial [0,4,4]) (fromRational (1%8))

orbit2 :: [CReal]
orbit2 = iterate ((4*) . fmap logMap2) (fromRational (1%8))

test :: Int -> IO ()
test k = mapM_ putStr .
         map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
         take k $ zip3 [0..] (orbit :: [Double]) (orbit :: [CReal])

test1 :: Int -> IO ()
test1 k = mapM_ putStr .
          map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
          take k $ zip3 [0..] (orbit :: [Double]) (orbit1 :: [CReal])

test2 :: Int -> IO ()
test2 k = mapM_ putStr .
          map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
          take k $ zip3 [0..] (orbit :: [Double]) (orbit2 :: [CReal])

logMapC :: (Fractional a) => Rational -> a -> a
logMapC c x = (fromRational c)*x*(1-x)

orbitC :: Fractional a => Rational -> [a]
orbitC c = iterate (logMapC c) (fromRational (1%8))

orbitC1 :: Rational -> [CReal]
orbitC1 c = iterate (((fromRational c) *) . polynomial [0,1,-1]) (fromRational (1%8))

testC :: Rational -> Int -> IO ()
testC c k = mapM_ putStr .
            map (\(n,d,r) -> printf "%3i %-20f %s\n" (n :: Int) (d :: Double) (showA $ require 32 r)) .
            take k $ zip3 [0..] (orbitC c:: [Double]) (orbitC c:: [CReal])
