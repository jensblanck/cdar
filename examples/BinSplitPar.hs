import Control.Monad.Par hiding (parMap)
import Data.Ratio

pqCombine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
pqCombine (pl, ql) (pr, qr) = (pl*qr+pr, ql*qr)

pq :: (Integer, Integer) -> (Integer, Integer)
pq (a, b)
    | d >   5 = let m = (a+b+1) `div` 2
                    pql = pq (a, m)
                    pqr = pq (m, b)
                in pqCombine pql pqr
    | otherwise = (sum $ scanl1 (*) [b,b-1..a+1], product [a+1..b])
  where d = b - a

main = print . flip seq () . (\(p,q) -> fromRational (p%q)) . runPar $ do --flip seq () . runPar $ do
  i <- new
  j <- new
  fork (put i (pq (0,160000)))
  fork (put j (pq (160000,320000)))
  a <- get i
  b <- get j
  return (pqCombine a b)
