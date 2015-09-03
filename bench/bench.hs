import Control.Concurrent
import Control.Monad
import Criterion.Main

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
      bench "Double" $ nf (!! 10000) (orbit 4 :: [Double])
    , bench "BR Approx" $ nf (require 10 . (!! 10000)) (orbit 4 :: [BR Approx])
    , bench "BR Approx (2)" $ nf (require 10 . (!! 10000)) (orbit2 4 :: [BR Approx])
    ],
    bgroup "Logistic 3.5" [
      bench "Double" $ nf (!! 10000) (orbit (7%2) :: [Double])
    , bench "BR Approx" $ nf (require 10 . (!! 10000)) (orbit (7%2) :: [BR Approx])
    , bench "BR Approx (2)" $ nf (require 10 . (!! 10000)) (orbit2 (7%2) :: [BR Approx])
    ],
    bgroup "Logistic 3" [
      bench "Double" $ nf (!! 10000) (orbit 3 :: [Double])
    , bench "BR Approx" $ nf (require 10 . (!! 10000)) (orbit 3 :: [BR Approx])
    , bench "BR Approx (2)" $ nf (require 10 . (!! 10000)) (orbit2 3 :: [BR Approx])
    ]
  ]

newSuite :: [Benchmark]
newSuite =
  [ bgroup "expappr"
    [ bench "double" $ nf exp (1 :: Double)
    , bench "20" $ nf (expA 20) 1
    , bench "20'" $ nf (expA' 20) 1
    , bench "20''" $ nf (expA'' 20) 1
    , bench "200" $ nf (expA 200) 1
    , bench "200'" $ nf (expA' 200) 1
    , bench "200''" $ nf (expA'' 200) 1
    , bench "2000" $ nf (expA 2000) 1
    , bench "2000'" $ nf (expA' 2000) 1
    , bench "2000''" $ nf (expA'' 2000) 1
    ]
  , bgroup "lnappr"
    [ bench "doubleLn" $ nf log (1.5 :: Double)
    , bench "20" $ nf (lnA 20) (Approx 3 0 (-1))
    , bench "20'" $ nf (lnA' 20) (Approx 3 0 (-1))
    , bench "20agm" $ nf (agmLnA (-20)) (Approx 3 0 (-1))
    , bench "200" $ nf (lnA 200) (Approx 3 0 (-1))
    , bench "200'" $ nf (lnA' 200) (Approx 3 0 (-1))
    , bench "200agm" $ nf (agmLnA (-200)) (Approx 3 0 (-1))
    , bench "2000" $ nf (lnA 2000) (Approx 3 0 (-1))
    , bench "2000'" $ nf (lnA' 2000) (Approx 3 0 (-1))
    , bench "2000agm" $ nf (agmLnA (-2000)) (Approx 3 0 (-1))
    ]
  , bgroup "exp"
    [ bench "double" $ nf exp (1 :: Double)
    , bench "20" $ nf (require 20 . exp) 1
    , bench "200" $ nf (require 200 . exp) 1
    ]
  , bgroup "ln"
    [ bench "double" $ nf log (2 :: Double)
    , bench "20" $ nf (require 20 . log) 2
    , bench "200" $ nf (require 200 . log) 2
    ]
  , bgroup "sin"
    [ bench "double" $ nf sin (1 :: Double)
    , bench "20" $ nf (require 20 . sin) 1
    , bench "200" $ nf (require 200 . sin) 1
    ]
  , bgroup "cos"
    [ bench "double" $ nf cos (1 :: Double)
    , bench "20" $ nf (require 20 . cos) 1
    , bench "200" $ nf (require 200 . cos) 1
    ]
  , bgroup "atan"
    [ bench "double" $ nf atan (1 :: Double)
    , bench "20" $ nf (require 20 . atan) 1
    , bench "200" $ nf (require 200 . atan) 1
    ]
  , bgroup "pi"
    [ bench "double" $ nf (\_ -> pi :: Double) (1 :: Double)
    , bench "20" $ nf (\_ -> require 20 $ pi) 1
    , bench "200" $ nf (\_ -> require 200 $ pi) 1
    ]
  , env setupEnv $ \ ~(pi1,pi2) ->
    bgroup "elementary Approx"
    [ bench "+ double" $ nf (\x -> x+x) (pi :: Double)
    , bench "* double" $ nf (\x -> x*x) (pi :: Double)
    , bench "rec double" $ nf (1/) (pi :: Double)
    , bench "+ 50" $ nf (\x -> x+x) pi1
    , bench "* 50" $ nf (\x -> x*x) pi1
    , bench "rec 50" $ nf (recipA 50) pi1
    , bench "+ 1000" $ nf (\x -> x+x) pi2
    , bench "* 1000" $ nf (\x -> x*x) pi2
    , bench "rec 1000" $ nf (recipA 1000) pi2
    ]
  ]

setupEnv = return . (\a -> (limitAndBound 50 a, a)) . limitAndBound 1000 . require 1000 $ pi

threadSuite :: MVar Approx -> MVar Approx -> [Benchmark]
threadSuite u v =
  [ bgroup "thread"
    [ bench "communicate" $ nfIO (do putMVar u (Approx 145324626 123 (-30)); a <- takeMVar v; return a)
    ]
  ]

main :: IO ()
main = defaultMain $ newSuite ++ suite

{- Are threads making criterion confused, times seem ok, but reported as unreliable.

threadSuite :: MVar Approx -> MVar Approx -> [Benchmark]
threadSuite u v =
  [ bgroup "thread"
    [ bench "communicate" $ nfIO (do putMVar u pi2; a <- takeMVar v; return a)
    ]
  ]

main :: IO ()
main = do
  u <- newEmptyMVar
  v <- newEmptyMVar
  forkIO $ server u v
  defaultMain $ threadSuite u v
  where server u v = forever $ do
          a <- takeMVar u
          putMVar v (a + 1)
-}
