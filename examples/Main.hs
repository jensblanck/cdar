module Main where

import Data.CDAR
--import Criterion.Main
--import Criterion.Config

main :: IO ()
main = putStrLn . showA . require 50 . sin $ pi / 2
