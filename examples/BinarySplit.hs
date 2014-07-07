module BinarySplit where

import Data.Ratio

import Data.CDAR

pq a b
    | b - a > 3 = let m = (a+b+1) `div` 2
                      (pam, qam) = pq a m
                      (pmb, qmb) = pq m b
                  in (pam*qmb+pmb, qam*qmb)
    | otherwise = (sum $ scanl1 (*) [b,b-1..a+1], product [a+1..b])

euler n = let (p,q) = pq 0 n
          in map showA . take 6 . (\(BR l) -> l) $ (fromIntegral p :: BR Approx) / fromIntegral q

