import Criterion.Main
import Criterion.Config

import Data.Bits
import Data.Ratio
import Data.CDAR

logMap :: (Fractional a) => Rational -> a -> a
logMap c x = (fromRational c)*x*(1-x)

logMap2 :: Approx -> Approx
logMap2 Bottom = Bottom
logMap2 (Approx m e s) =
    let one = bit (-s)
        t = abs (bit (-s-1) - m)
    in boundErrorTerm $
       if t >= e
       then Approx (m*(one-m)-e^2) (e * abs (one-2*m)) (2*s)
       else Approx (bit (-2*s - 2)) ((t + e)^2) (2*s)

orbit :: Fractional a => Rational -> [a]
orbit c = iterate (logMap c) (fromRational (1%8))

orbit2 :: Rational -> [BR Approx]
orbit2 c = iterate (((fromRational c)*) . fmap logMap2) (fromRational (1%8))

suite :: [Benchmark]
suite = [
    bgroup "Logistic 4" [
      bench "Double" $ whnf (!! 10000) (orbit 4 :: [Double])
    , bench "BR Approx" $ whnf (require 10 . (!! 10000)) (orbit 4 :: [BR Approx])
    , bench "BR Approx (2)" $ whnf (require 10 . (!! 10000)) (orbit2 4 :: [BR Approx])
    ],
    bgroup "Logistic 3.5" [
      bench "Double" $ whnf (!! 10000) (orbit (7%2) :: [Double])
    , bench "BR Approx" $ whnf (require 10 . (!! 10000)) (orbit (7%2) :: [BR Approx])
    , bench "BR Approx (2)" $ whnf (require 10 . (!! 10000)) (orbit2 (7%2) :: [BR Approx])
    ],
    bgroup "Logistic 3" [
      bench "Double" $ whnf (!! 10000) (orbit 3 :: [Double])
    , bench "BR Approx" $ whnf (require 10 . (!! 10000)) (orbit 3 :: [BR Approx])
    , bench "BR Approx (2)" $ whnf (require 10 . (!! 10000)) (orbit2 3 :: [BR Approx])
    ]
  ]

main :: IO ()
main = defaultMain suite
