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
    , bench "B40" $ nf (expBinarySplittingA 40) 1
    , bench "T40" $ nf (expTaylorA 40) 1
    , bench "B400" $ nf (expBinarySplittingA 400) 1
    , bench "T400" $ nf (expTaylorA 400) 1
    , bench "B4000" $ nf (expBinarySplittingA 4000) 1
    , bench "T4000" $ nf (expTaylorA 4000) 1
    ]
  , bgroup "logappr"
    [ bench "doubleLog" $ nf log (1.5 :: Double)
    , bench "40B" $ nf (logBinarySplittingA 40) (Approx 3 0 (-1))
    , bench "40T" $ nf (logTaylorA 40) (Approx 3 0 (-1))
    , bench "40agm" $ nf (logAgmA (-40)) (Approx 3 0 (-1))
    , bench "400B" $ nf (logBinarySplittingA 400) (Approx 3 0 (-1))
    , bench "400T" $ nf (logTaylorA 400) (Approx 3 0 (-1))
    , bench "400agm" $ nf (logAgmA (-400)) (Approx 3 0 (-1))
    , bench "4000B" $ nf (logBinarySplittingA 4000) (Approx 3 0 (-1))
    , bench "4000T" $ nf (logTaylorA 4000) (Approx 3 0 (-1))
    , bench "4000agm" $ nf (logAgmA (-4000)) (Approx 3 0 (-1))
    ]
  , bgroup "exp"
    [ bench "double" $ nf exp (1 :: Double)
    , bench "40" $ nf (require 40 . exp) 1
    , bench "400" $ nf (require 400 . exp) 1
    ]
  , bgroup "log"
    [ bench "double" $ nf log (2 :: Double)
    , bench "40" $ nf (require 40 . log) 2
    , bench "400" $ nf (require 400 . log) 2
    ]
  , bgroup "sin"
    [ bench "double" $ nf sin (1 :: Double)
    , bench "40" $ nf (require 40 . sin) 1
    , bench "40T" $ nf (sinTaylorA 40) 1
    , bench "40BR" $ nf (require 40 . sinBR) 1
    , bench "400" $ nf (require 400 . sin) 1
    , bench "400T" $ nf (sinTaylorA 400) 1
    , bench "400BR" $ nf (require 400 . sinBR) 1
    ]
  , bgroup "cos"
    [ bench "double" $ nf cos (1 :: Double)
    , bench "40" $ nf (require 40 . cos) 1
    , bench "400" $ nf (require 400 . cos) 1
    ]
  , bgroup "atan"
    [ bench "double" $ nf atan (1 :: Double)
    , bench "40" $ nf (require 40 . atan) 1
    , bench "400" $ nf (require 400 . atan) 1
    ]
  , bgroup "pi"
    [ bench "double" $ nf (\_ -> pi :: Double) (1 :: Double)
    , bench "40" $ nf (\_ -> require 40 $ pi) 1
    , bench "400" $ nf (\_ -> require 400 $ pi) 1
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
