import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Bits
import Data.Ratio

import Data.CDAR

pq :: Integer -> Integer -> (Integer, Integer)
pq a b
    | d > 2000= let m = (a+b+1) `div` 2
                    ((pam, qam), (pmb, qmb)) = (pq a m, pq m b) `using` parTuple2 rpar rpar --rdeepseq rdeepseq
                in (pam*qmb+pmb, qam*qmb) `using` parTuple2 rpar rpar --rdeepseq rdeepseq
    | d >   8 = let m = (a+b+1) `div` 2
                    (pam, qam) = pq a m
                    (pmb, qmb) = pq m b
                in (pam*qmb+pmb, qam*qmb) -- `using` parTuple2 rdeepseq rdeepseq
    | otherwise = (sum $ scanl1 (*) [b,b-1..a+1], product [a+1..b])
  where d = b - a

fudge :: Approx -> Approx -> Approx
fudge (Approx m e s) (Approx m' e' s') =
    let m'' = 1 + (abs m' + e') `shift` (s' - s + 1)
    in Approx m (e+m'') s

euler n =
    let (p,q) = pq 0 n
        a = (fromIntegral p :: Approx) / fromIntegral q
        e = (1 :: Approx) / fromIntegral (q*(n+1))
    in boundErrorTerm $ fudge a e

main = print $ euler 200000 `seq` ()
