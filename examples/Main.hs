module Main where

import Data.CDAR

main = putStrLn . showA . require 50 . sin $ pi / 2
