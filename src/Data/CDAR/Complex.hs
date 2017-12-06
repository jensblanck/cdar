{-# LANGUAGE BangPatterns,GADTs,TypeSynonymInstances,FlexibleInstances #-}
{-|
= Computable Complex Arithmetic
This module provides the data type 'CC' that implements the closed field of
computable complex numbers.

== Centred Dyadic Approximations The computable complex numbers are realised
as lists of rapidly shrinking rectilinear boxes. The boxes used here are
centred dyadic boxes, implemented here as the data type 'CApprox'.

For more information on the theoretical aspects see <http://cs.swan.ac.uk/~csjens/pdf/centred.pdf>.
-}
module Data.CDAR.Complex (CApprox(..)
                         ,CC(..)
--                        ,errorBits
--                        ,errorBound
--                        ,defaultPrecision
                         ) where

import           Control.Applicative (ZipList (..))
--import           Control.DeepSeq
--import           Control.Exception
import           Data.Bits
import           Data.Char (isDigit)
import           Data.CDAR.Approx
import           Data.CDAR.Classes
import           Data.CDAR.Dyadic
import           Data.CDAR.Extended
import           Data.CDAR.IntegerLog
import           Data.Char (intToDigit)
import           Data.List (findIndex, intersperse, transpose, unfoldr, zipWith4)
import           Data.Ratio

import Debug.Trace

{-|
= Centred Dyadic Approximations
There are two constructors for approximations:

- 'CApprox' is encodes some finite box with dyadic endpoints. A real
  number is /approximated/ by the approximation is it belongs to the box.
- 'CBottom' is the trivial approximation that approximates all complex numbers.

The three fields of an @CApprox x y e s@ should be thought of as:

[@x@] the real part of the midpoint
[@y@] the imaginary part of the midpoint
[@e@] the error term
[@s@] the exponent

Thus, a value @CApprox x y e s@ is to be interpreted as the box
[((x-e)+i(y-e))*2^s, ((x+e)+i(y+e))*2^s].

== Potential for overflow
Since the third field (the exponent) is only an 'Int' it may overflow. This is
an optimisation that was adopted since it seems unlikely that overflow in a 64
bit Int exponent would occur. In a 32 bit system, this is potentially an
issue.

The 'Integer' data type is unbonded, but is, of course, bounded by the
available memory available in the computer. No attempt has been made to check
for exhausted memory.

== Approximations as a Domain

Ordered by reverse inclusion the dyadic intervals encoded by the 'Approx'
approximations (including 'Bottom') constitute the compact elements of a Scott
domain /D/. (It is a substructure of the (algebraic) interval domain.)
We will identify our approximations with the compact elements of /D/.

Increasing sequences in /D/ have suprema. A sequence /converges/ if the length
of the approximations tend to zero. The supremum of a converging sequence is a
singleton set containing a real number. Let ρ be the map taking a converging
sequence to the unique real number in the supremum. The computations on
(computable) real numbers is via this representation map ρ.

There is no check that the sequences we have are in fact increasing, but we
are assuming that all sequences are pairwise consistent. We can thus create an
increasing sequence by considering the sequence of finite suprema. For
correctness, we have to ensure that all operations done on consistent
sequences result in consistent sequences. If non-consistent sequences are
somehow input we can make no guarantees at all about the computed value.

Note, that we cannot ensure that converging sequences are mapped to converging
sequences because of properties of computable real arithmetic. In particular,
at any discuntinuity, it is impossible to compute a converging sequence.
-}
data CApprox = CApprox Integer Integer Integer Int
             | CBottom
             deriving (Read,Show)

instance NFData CApprox where
    rnf CBottom = ()
    rnf (CApprox x y e s) = rnf x `seq` rnf y `seq` rnf e `seq` rnf s

instance Scalable CApprox where
  scale CBottom _ = CBottom
  scale (CApprox x y e s) n = CApprox x y e (s+n)

instance Scalable CC where
  scale (CC z) n = CC $ flip scale n <$> z

{-|
=The Computable Complex data type

Computable Complex are realised as infinite sequences of centred dyadic
representations.

All approximations in such a sequence should be pairwise consistent, i.e.,
have a non-empty intersection. However, there is no check that this is
actually the case.

If the diameter of the approximations tend to zero we say that the sequences
converges to the unique real number in the intersection of all intervals.
Given the domain /D/ of approximations described in 'Approx', we have a
representation (a retraction) ρ from the converging sequences in /D/ to ℝ.
Some operations on computable reals are partial, notably equality and
ordering. A consequence of this is that there is no guarantee that a
computable real will converge.

For the /n/-th element in the sequence there is a bound on how much effort is
put into the computation of the approximation. For involved computations it is
possible that several of the leading approximations are trivial, i.e.,
'Bottom'. If the computation will eventually converge, it will generate proper
approximation after a modest number of initial trivial approximations.

The amount of added effort in each iteration is rather substantial so the
expected precision of approximations increase very quickly.

==The actual data type

In fact, the type 'CR' is a newtype of 'ZipList' 'Approx' in the
implementation of infinite sequences of approximations, as that allows for
applicative style. Hopefully, it is not needed to access the internal
representation of 'CR' directly.
-}
newtype CC = CC {unCC :: ZipList CApprox}

-- |Number of bits that error term is allowed to take up. A larger size allows
-- for more precise but slightly more costly computations. The value here is
-- suggested by test runs.
errorBits :: Int
errorBits = 10

errorBound :: Integer
errorBound = 2^errorBits

