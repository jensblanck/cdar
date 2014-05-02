import Criterion.Main
import Criterion.Config

import Data.Ratio
import Data.CDAR

logMap :: (Fractional a) => Rational -> a -> a
logMap c x = (fromRational c)*x*(1-x)

orbit :: Fractional a => Rational -> [a]
orbit c = iterate (logMap c) (fromRational (1%8))

suite :: [Benchmark]
suite = [
    bgroup "Logistic 4" [
      bench "Double" $ whnf (!! 10000) (orbit 4 :: [Double])
    , bench "BR Approx" $ whnf (require 10 . (!! 10000)) (orbit 4 :: [BR Approx])
    ],
    bgroup "Logistic 3.5" [
      bench "Double" $ whnf (!! 10000) (orbit (7%2) :: [Double])
    , bench "BR Approx" $ whnf (require 10 . (!! 10000)) (orbit (7%2) :: [BR Approx])
    ],
    bgroup "Logistic 3" [
      bench "Double" $ whnf (!! 10000) (orbit 3 :: [Double])
    , bench "BR Approx" $ whnf (require 10 . (!! 10000)) (orbit 3 :: [BR Approx])
    ]
  ]

main :: IO ()
main = defaultMain suite
