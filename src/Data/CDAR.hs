{-| Convenience module importing and reexporting the 'CReal' data type of
computable real numbers and the underlying data type including the centred
dyadic approximations 'Approx'.

Modules imported are:

- "Data.CDAR.Approx"
- "Data.CDAR.Dyadic"
- "Data.CDAR.Extended"
- "Data.CDAR.Interval"
- "Data.CDAR.IntegerLog"
- "Data.CDAR.Interval"
- "Data.CDAR.POrd"
-}
module Data.CDAR (module Approx
                 ,module Dyadic
                 ,module Extended
                 ,module IntegerLog
                 ,module Interval
                 ,module POrd) where

import           Data.CDAR.Approx as Approx
import           Data.CDAR.Dyadic as Dyadic
import           Data.CDAR.Extended as Extended
import           Data.CDAR.IntegerLog as IntegerLog
import           Data.CDAR.Interval as Interval hiding (approximatedBy, better, centre, diameter, exact, lowerBound, upperBound)
import           Data.CDAR.POrd as POrd
