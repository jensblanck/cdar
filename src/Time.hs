import Text.Printf
import Control.Exception
import System.CPUTime
import Data.CDAR.BR
import Data.CDAR.CR
import Data.CDAR.Approx hiding (toDouble)
import System.Environment

c,c' :: Fractional a => a
c = 4
c' = 1961/512

x0 :: Fractional a => a
x0 = 1/8

f c x = c*x*(1-x)
g c x = c * polynomial [0,1,-1] x
h c x = c^2 * polynomial [0,1,-1-c,2*c,-c] x
h4 x = 16 * polynomial [0,1,-5,8,-4] x

fc c x = CRLet "x" x (c*(CRVar "x")*(1-(CRVar "x")))
gc c x = c * CRPoly [0,1,-1] x

time :: String -> IO t -> IO t
time s a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf (s ++ "-- Computation time: %0.6f sec\n") (diff :: Double)
    return v
 
main = do
  [d] <- map read `fmap` getArgs
  putStrLn "Starting..."
  time "seq mul  4 " $ (print . Data.CDAR.BR.toDouble . (!!d) $ iterate (f c) x0)
  time "seq poly 4 " $ (print . Data.CDAR.BR.toDouble . (!!d) $ iterate (g c) x0)
  time "seq poly h " $ (print . Data.CDAR.BR.toDouble . (!!(d `div` 2)) $ iterate (h c) x0)
  time "seq poly h4" $ (print . Data.CDAR.BR.toDouble . (!!(d `div` 2)) $ iterate (h4) x0)
  time "let mul  4 " $ (print . Data.CDAR.CR.toDouble . (!!d) $ iterate (fc c) x0)
  time "let poly 4 " $ (print . Data.CDAR.CR.toDouble . (!!d) $ iterate (gc c) x0)
  time "double   4 " $ (print . (!!d) $ iterate (f c) x0)
  time "seq mul  c " $ (print . Data.CDAR.BR.toDouble . (!!d) $ iterate (f c') x0)
  time "seq poly c " $ (print . Data.CDAR.BR.toDouble . (!!d) $ iterate (g c') x0)
  time "seq poly h " $ (print . Data.CDAR.BR.toDouble . (!!(d `div` 2)) $ iterate (h c') x0)
  time "let mul  c " $ (print . Data.CDAR.CR.toDouble . (!!d) $ iterate (fc c') x0)
  time "let poly c " $ (print . Data.CDAR.CR.toDouble . (!!d) $ iterate (gc c') x0)
  time "double   c " $ (print . (!!d) $ iterate (f c') x0)
--  time "piAMachin  " $ print (piAMachin (d*10))
  time "piMachinA  " $ print (piMachinA (-d*10))
--  time "ln2A'      " $ print (ln2A' (d*10))
  time "ln2A       " $ print (ln2A (-d*10))
  putStrLn "Done."
