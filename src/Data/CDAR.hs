{-| Convenience module importing and reexporting the 'CReal' data type of
computable real numbers and the underlying data type including the centred
dyadic approximations 'Approx'.

Modules imported are:

- "Data.CDAR.Approx"
- "Data.CDAR.Classes"
- "Data.CDAR.Dyadic"
- "Data.CDAR.Extended"
- "Data.CDAR.IntegerLog"
- "Data.CDAR.POrd"
-}
module Data.CDAR (module Approx
                 ,module Classes
                 ,module Dyadic
                 ,module Extended
                 ,module IntegerLog
                 ,module POrd) where

import           Data.CDAR.Approx as Approx
import           Data.CDAR.Classes as Classes
import           Data.CDAR.Dyadic as Dyadic
import           Data.CDAR.Extended as Extended
import           Data.CDAR.IntegerLog as IntegerLog
import           Data.CDAR.POrd as POrd
