# CDAR

Implementation of computable real numbers, which is also often referred to as
Exact Real Arithmetic.

## Computable Real Numbers

Computable real numbers are a countable subset of the real numbers. It
contains all real numbers that can be described by a finite program.  This
includes all numbers that are commonly used in mathematics, including π and e.
It is also closed under all field operations and all common transcendental
functions such as exponential and trigonometric functions.

## Interval Arithmetic and Computable Real numbers

This implementation used interval arithmetic internally but that doesn't mean
that this is interval arithmetic. Computable real numbers are exact, so we are
working under the assumption that arbitrary good approximations of input
values can be found. This is obviously different from interval arithmetic
where part of the point is that input values may not be exact.

## This implementation

Is based on Centred Dyadic Approximations as described in [Blanck
2006](http://cs.swan.ac.uk/~csjens/pdf/centred.pdf).

It is also heavily inspired by the [iRRAM](http://irram.uni-trier.de/)
implementation by Norbert Müller. In particular, it uses nested intervals
rather than Cauchy sequences with a computable modulus function.

This is implementation should have comparable efficiency to implementations
using Cauchy sequences for shallow expressions. However, for deeply nested
expressions, such as iterated functions, it should be significantly faster.

Each computable real can be viewed as an infinite list of rapidly shrinking
intervals. Thus, laziness of Haskell is used to keep the computations finite.

## Other Haskell Implementations

* ERA (Can't find a link at the moment)

## Installation

Should build under `stack`.

## Motivation

Although the terminology Exact Real Arithmetic promises the ability to compute
arbitrarily the result with huge precision, such as π with 10000 digits or
more, this is not the real strength of this implementation. Rather, if you
would like 10 correct digits (and in truth, even that many is often more than
we need) then you ask for 10 digits from the resulting computable real number
and even if the computation has to compute intermediate values with much
higher precision it will return those 10 digits.

A word of warning though: Some operations are partial. In particular,
comparisons are partial, and so is 1/x near 0.

## Examples

