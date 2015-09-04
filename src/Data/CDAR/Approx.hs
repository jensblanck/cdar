module Data.CDAR.Approx (Approx(..)
                        ,errorBits
                        ,errorBound
                        ,defaultPrecision
                        ,showA
                        ,showInBaseA
                        ,toEDI
                        ,fromEDI
                        ,lowerBound
                        ,upperBound
                        ,centre
                        ,diameter
                        ,exact
                        ,approximatedBy
                        ,better
                        ,fromDyadic
                        ,toApprox
                        ,recipA
                        ,modA
                        ,divModA
                        ,toDouble
                        ,toDouble2
                        ,precision
                        ,significance
                        ,boundErrorTerm
                        ,limitSize
                        ,checkPrecisionLeft
                        ,limitAndBound
                        ,poly
                        ,pow
                        ,powers
                        ,sqrtA
                        ,sqrtD
                        ,shiftD
                        ,sqrA
                        ,log2Factorials
                        ,expA
                        ,expBinarySplittingA
                        ,expTaylorA
                        ,logA
                        ,logBinarySplittingA
                        ,logTaylorA
                        ,sinA
                        ,cosA
                        ,atanA
                        ,piRaw
                        ,nonZeroCentred
                        ,piMachinA
                        ,piBorweinA
                        ,piAgmA
                        ,log2A
                        ,logAgmA
                        ,agmLn) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Parallel.Strategies
import           Data.Bits
import           Data.CDAR.Dyadic hiding (normalise)
import           Data.CDAR.Extended
import           Data.CDAR.IntegerLog
import qualified Data.CDAR.Interval as Interval
import           Data.CDAR.POrd
import           Data.Char (intToDigit)
import           Data.List (findIndex, unfoldr, zipWith4)
import           Data.Ratio

type EDI = Interval.Interval (Extended Dyadic)
type Precision = Int    -- Precition measures how many bits after the binary point.

-- Centred dyadic approximations

data Approx = Approx Integer Integer Int
            | Bottom
              deriving (Read,Show)

instance NFData Approx where
    rnf Bottom = ()
    rnf (Approx m e s) = rnf m `seq` rnf e `seq` rnf s

-- |Number of bits that error term is allowed to take up. A larger size allows
-- for more precise but slightly more costly computations. The value here is
-- suggested by test runs.
errorBits :: Int
errorBits = 10

errorBound :: Integer
errorBound = 2^errorBits

-- |The default cutoff for diverging computations. May well be chosen much
-- smaller. 31 corresponds to about 10 decimal places.
defaultPrecision :: Precision
defaultPrecision = 31

showA :: Approx -> String
showA = showInBaseA 10

-- |Allows to show an `Approx` in bases up to 16.
{- am is the absolute value of the significand
   b corresponds to the value 1 with respect to the shift s -- this is used to find the digits in the auxiliary functions
   i is the integral part of am
   f is the fractional part of am
   i' and f' are the integral and fractional parts relevant for near zero approximations
   e' is the error term shifted appropriately when s positive, also set to at least 1
     (otherwise odd bases will yield infinite expansions)
-}
showInBaseA :: Int -> Approx -> String
showInBaseA _ Bottom = "⊥"
showInBaseA base (Approx m e s)
    | e == 0 && (even base || s >= 0)
                     = sign ++ showExactA base b i f
    | am < e         = "±" ++ showNearZeroA base b i' f'
    | otherwise      = sign ++ showInexactA base b i f e'
    where b = bit (max 0 (-s))
          am = abs m
          i = shift am s
          e' = max 1 $ shift e (max 0 s)
          f = am .&. (b-1)
          i' = shift (am+e) s
          f' = (am+e) .&. (b-1)
          sign = if m < 0 then "-" else ""

showExactA :: Int -> Integer -> Integer -> Integer -> String
showExactA base b i f = 
    let g i' = let (q,r) = quotRem i' (fromIntegral base)
               in if i' == 0 then Nothing
                  else Just (intToDigit (fromIntegral r), q)
        ip = reverse (unfoldr g i)
        h f' = let (q,r) = quotRem ((fromIntegral base)*f') b
               in if f' == 0 then Nothing
                  else Just (intToDigit (fromIntegral q), r)
        fp = unfoldr h f
    in (if null ip then "0" else ip)
       ++ (if null fp then "" else ".")
       ++ fp

showNearZeroA :: Int -> Integer -> Integer -> Integer -> String
showNearZeroA base b i f =
    let s = showExactA base b i f
        t = takeWhile (flip elem "0.~") s
        u = takeWhile (/= '.') s
    in if null t
       then replicate (length u) '~'
       else t ++ "~"

showInexactA :: Int -> Integer -> Integer -> Integer -> Integer -> String
showInexactA base b i f e =
    let g (0,b',f') = if b' < f'+e
                      then Just ('1', (0, (fromIntegral base)*b', f'))
                      else Nothing
        g (n,b',f') = let (q,r) = quotRem n (fromIntegral base)
                          z = (q, (fromIntegral base)*b', r*b'+f')
                      in if e+f' <= b'
                         then Just (intToDigit (fromIntegral r), z)
                         else if e <= min f' b'
                              then Just (intToDigit ((fromIntegral r + 1) `rem` (fromIntegral base)), z)
                              else Just ('~', z)
        intRev = unfoldr g (i,b,f)
        noFrac = case intRev of
                   [] -> False
                   (x:_) -> x == '~'
        int = if null intRev then "0" else reverse intRev
        h (f',err) = let (q,r) = quotRem ((fromIntegral base)*f') b
                         err' = (fromIntegral base)*err
                         z = (r, err')
                     in if err' + r <= b
                        then Just (intToDigit (fromIntegral q), z)
                        else if err' <= min r b
                             then Just (intToDigit ((fromIntegral q + 1) `rem` (fromIntegral base)), z)
                             else Nothing
        frac = unfoldr h (f,e)
    in int ++ if noFrac
              then ""
              else "." ++ frac ++ "~"

toEDI :: Approx -> EDI
toEDI (Approx m e s) = Interval.Interval (Finite ((m-e):^s)) (Finite ((m+e):^s))
toEDI _ = Interval.Interval NegInf PosInf

fromEDI :: EDI -> Approx
fromEDI (Interval.Interval (Finite l) (Finite u)) =
    let a@(m:^s) = Interval.average l u
        (n:^t)   = u-a
        r        = min s t
        m'       = unsafeShiftL m (s-r)
        n'       = unsafeShiftL n (t-r)
    in (Approx m' n' r)
fromEDI _ = Bottom

-- Interval operations
lowerBound :: Approx -> Extended Dyadic
lowerBound (Approx m e s) = Finite ((m-e):^s)
lowerBound Bottom = NegInf

upperBound :: Approx -> Extended Dyadic
upperBound (Approx m e s) = Finite ((m+e):^s)
upperBound Bottom = PosInf

centre :: Approx -> Dyadic
centre (Approx m _ s) = (m:^s)
centre _ = undefined

radius :: Approx -> Dyadic
radius (Approx _ e s) = e:^s
radius _ = undefined

diameter :: Approx -> Extended Dyadic
diameter (Approx _ e s) = Finite $ 2 * (e:^s)
diameter _ = PosInf

exact :: Approx -> Bool
exact (Approx _ 0 _) = True
exact _ = False

approximatedBy :: Real a => a -> Approx -> Bool
_ `approximatedBy` Bottom = True
r `approximatedBy` d =
    let r' = toRational r
    in toRational (lowerBound d) <= r' && r' <= toRational (upperBound d)

better :: Approx -> Approx -> Bool
d `better` e = lowerBound d >= lowerBound e &&
               upperBound d <= upperBound e

fromDyadic :: Dyadic -> Approx
fromDyadic (m:^s) = Approx m 0 s

instance Eq Approx where
    (Approx m e s) == (Approx n f t)
        | s >= t = let k = s-t
                   in unsafeShiftL m k == n && unsafeShiftL e k == f
        | s <  t = let k = t-s
                   in m == unsafeShiftL n k && e == unsafeShiftL f k
    Bottom == Bottom = True
    _ == _ = False

instance Enum Approx where
    toEnum n = Approx (fromIntegral n) 0 0
    fromEnum (Approx m _ s) = fromIntegral $ shift m s
    fromEnum Bottom = 0

instance Num Approx where
    (Approx m e s) + (Approx n f t)
        | s >= t = let k = s-t
                   in Approx (unsafeShiftL m k + n) (unsafeShiftL e k + f) t
        | s <  t = let k = t-s
                   in Approx (m + unsafeShiftL n k) (e + unsafeShiftL f k) s
    _ + _ = Bottom
    (Approx m e s) * (Approx n f t)
        | am >= e && an >= f && a > 0           = Approx (a+d) (ab+ac) u
        | am >= e && an >= f && a < 0           = Approx (a-d) (ab+ac) u
        | am < e && n >= f                      = Approx (a+b) (ac+d) u
        | am < e && -n >= f                     = Approx (a-b) (ac+d) u
        | m >= e && an < f                      = Approx (a+c) (ab+d) u
        | -m >= e && an < f                     = Approx (a-c) (ab+d) u
        | a == 0                                = Approx (0) (ab+ac+d) u
        | am < e && an < f && a > 0 && ab > ac  = Approx (a+ac) (ab+d) u
        | am < e && an < f && a > 0 && ab <= ac = Approx (a+ab) (ac+d) u
        | am < e && an < f && a < 0 && ab > ac  = Approx (a-ac) (ab+d) u
        | am < e && an < f && a < 0 && ab <= ac = Approx (a-ab) (ac+d) u
      where am = (abs m)
            an = (abs n)
            a = m*n
            b = m*f
            c = n*e
            d = e*f
            ab = (abs b)
            ac = (abs c)
            u = s+t
    _ * _ = Bottom
    negate (Approx m e s) = Approx (-m) e s
    negate Bottom = Bottom
    abs (Approx m e s)
        | m' < e    = let e' = unsafeShiftR (m'+e+1) 1
                      in Approx e' e' s
        | otherwise = Approx m' e s
      where m' = abs m
    abs Bottom = Bottom
    signum (Approx m e _)
        | e == 0 = Approx (signum m) 0 0
        | abs m < e = Approx 0 1 0
        | abs m == e = Approx (signum m) 1 (-1)
        | otherwise = Approx (signum m) 0 0
    signum Bottom = Approx 0 1 0
    fromInteger i = Approx i 0 0

toApprox :: Precision -> Rational -> Approx
toApprox t r = Approx (2 * round (r*2^^t)) 1 (-t - 1)

-- |Not a proper Fractional type as Approx are intervals
instance Fractional Approx where
    fromRational = toApprox defaultPrecision
    recip = recipA defaultPrecision

recipA :: Precision -> Approx -> Approx
recipA _ Bottom = Bottom
recipA t (Approx m e s)
    | e == 0      = let s' = 2*s + t + 5 + integerLog2 (abs m)
                    in Approx
                         (round (bit (s'-2*s) % m))
                         1
                         (s-s')
    | (abs m) > e = let d = m*m-e*e
                        d2 = unsafeShiftR d 1
                        s' = 2 * (integerLog2 m + errorBits)
                    in boundErrorTerm $ Approx
                           ((unsafeShiftL m s' + d2) `div` d)
                           ((unsafeShiftL e s' + d2) `div` d)
                           (-s-s')
    -- | (abs m) > e = let d = m*m-e*e
    --                     s' = 2 * (integerLog2 m + errorBits)
    --                 in boundErrorTerm $ Approx
    --                        (round (unsafeShiftL m s'%(d)))
    --                        (ceiling (1%2 + unsafeShiftL e s'%(d)))
    --                        (-s-s')
    | otherwise   = Bottom

divAInteger :: Approx -> Integer -> Approx
divAInteger (Approx m e s) n =
  let t = integerLog2 n
  in Approx (round (unsafeShiftL m t % n))
             (ceiling (unsafeShiftL e t % n))
             s

modA :: Approx -> Approx -> Approx
modA (Approx m e s) (Approx n f t) =
    let r = min s t
        (d,m') = divMod (unsafeShiftL m (s-r)) (unsafeShiftL n (t-r))
        e' = e + abs d * f
    in Approx m' e' r
modA _ _ = Bottom

divModA :: Approx -> Approx -> (Approx, Approx)
divModA (Approx m e s) (Approx n f t) =
    let r = min s t
        (d,m') = divMod (unsafeShiftL m (s-r)) (unsafeShiftL n (t-r))
        e' = e + abs d * f
    in (fromIntegral d, Approx m' e' r)
divModA _ _ = (Bottom, Bottom)

-- |Not a proper Ord type as Approx are intervals
instance Ord Approx where
    compare (Approx m e s) (Approx n f t)
        | abs ((m:^s)-(n:^t)) > (e:^s)+(f:^t) = compare (m:^s) (n:^t)
        | otherwise                           = undefined
    compare _ _ = undefined

instance IntervalOrd Approx where
    intervalCompare a b = intervalCompare (toEDI a) (toEDI b)

instance PartialOrd Approx where
    partialCompare a b = f $ intervalCompare a b
        where f Equal = Just EQ
              f LessThan = Just LT
              f GreaterThan = Just GT
              f _ = Nothing

instance Real Approx where
    toRational (Approx m e s) = approxRational
                                  (toRational (m:^s))
                                  (toRational (e:^s))
    toRational _ = undefined

toDouble :: Approx -> Double
toDouble = fromRational . toRational . centre

toDouble2 :: Approx -> Interval.Interval Double
toDouble2 = fmap (fromRational . toRational) . toEDI

precision :: Approx -> Extended Precision
precision (Approx _ 0 _) = PosInf
precision (Approx _ e s) = Finite $ - s - (integerLog2 e) - 1
precision Bottom         = NegInf

significance :: Approx -> Extended Int
significance (Approx _ 0 _) = PosInf
significance (Approx 0 _ _) = NegInf
significance (Approx m 1 _) =  Finite $ integerLog2 (abs m) - 1
significance (Approx m e _) =
    Finite $ (integerLog2 (abs m)) - (integerLog2 (e-1)) - 1
significance Bottom         = NegInf

boundErrorTerm :: Approx -> Approx
boundErrorTerm Bottom = Bottom
boundErrorTerm a@(Approx m e s)
    | e < errorBound = a
    | otherwise =
        let k = integerLog2 e + 1 - errorBits
            t = testBit m (k-1)
            m' = unsafeShiftR m k
            -- may overflow and use errorBits+1
            e' = unsafeShiftR (e + bit (k-1)) k + 1 
        in if t
           then Approx (m'+1) e' (s+k)
           else Approx m'     e' (s+k)

limitSize :: Precision -> Approx -> Approx
limitSize _ Bottom = Bottom
limitSize l a@(Approx m e s)
    | k > 0     = Approx
                  ((if testBit m (k-1) then (+1) else id) (unsafeShiftR m k))
                  (1 + (unsafeShiftR (e + bit (k-1)) k))
                  (-l)
    | otherwise = a
    where k = (-s)-l

checkPrecisionLeft :: Approx -> Approx
checkPrecisionLeft a
        | precision a > pure defaultPrecision = a
        | otherwise = throw $ LossOfPrecision

limitAndBound :: Precision -> Approx -> Approx
limitAndBound limit =
    limitSize limit . boundErrorTerm

poly :: [Approx] -> Approx -> Approx
poly [] _ = 0
poly _ Bottom = Bottom
poly as x =
    let --poly' :: [Dyadic] -> Dyadic -> Dyadic
        poly' as' x' = sum . zipWith (*) as' $ pow x'
        ms = map centre as
        (m':^s) = poly' ms (centre x)
        ds = zipWith (*) (tail as) (map fromIntegral ([1,2..] :: [Int]))
        (Finite b) = upperBound . abs $ poly' ds x
        (e':^_) = radius x * b
        -- exponent above will be same as s
    in Approx m' e' s

pow :: (Num a) => a -> [a]
pow x = iterate (* x) 1

binomialCoefficients :: (Num a) => [[a]]
binomialCoefficients =
    let f ss = 1 : zipWith (+) ss (tail ss) ++ [1]
    in iterate f [1]

powers :: Approx -> [Approx]
powers (Approx m e s) =
    let ms = pow m
        es = pow e
        f = reverse . zipWith (*) ms . reverse . zipWith (*) es
        sumAlt [] = (0,0)
        sumAlt (x:[]) = (x,0)
        sumAlt (x:y:xs) = let (a,b) = sumAlt xs in (a+x,b+y)
        g s' (m', e') = Approx m' e' s'
    in zipWith g (iterate (+s) 0) $ map (sumAlt . f) binomialCoefficients
powers _ = repeat Bottom

sqrtA :: Precision -> Approx -> Approx
sqrtA _ Bottom = Bottom
sqrtA k a@(Approx m e s)
    | -m > e    = error "Attempting sqrt of Approx containing only negative numbers."
    | m < e     = Bottom
    | e == 0    = let (n:^t) = shiftD (-k) $ sqrtD (-k-2) (m:^s)
                  in Approx n 1 t
    | m == e    = let (n:^t) = sqrtD (s `quot` 2 -errorBits) ((m+e):^s)
                      n' = (n+2) `quot` 2
                  in Approx n' n' t
    | otherwise = let (Finite p) = significance a
                      s' = s `quot` 2 - p - errorBits
                      l@(n:^t) = sqrtD s' ((m-e):^s)
                      (n':^t') = sqrtD' s' ((m+e):^s) l
                  in fromEDI $ Interval.Interval (Finite ((n-1):^t)) (Finite ((n'+1):^t'))

sqrA :: Approx -> Approx
sqrA (Approx m e s) = Approx (m^2 + e^2) (2*abs m*e) (2*s)

-- Binary splitting

abpq :: Num a => [Integer] -> [Integer] -> [a] -> [a] -> Int -> Int -> (a, a, Integer, a)
abpq as bs ps qs n1 n2
    | n == 1 = (ps !! n1, qs !! n1, bs !! n1, fromIntegral (as !! n1) * ps !! n1)
    | n < 6  = let as' = take n $ drop n1 as
                   bs' = take n $ drop n1 bs
                   ps' = take n $ drop n1 ps
                   qs' = take n $ drop n1 qs
                   pbs = product bs'
                   bs'' = map (pbs `div`) bs'
                   ps'' = scanl1 (*) ps'
                   qs'' = scanr1 (*) (tail qs' ++ [1])
               in (ps'' !! (n-1), product qs', pbs
                  , sum $ zipWith4 (\a b p q -> fromIntegral a * fromIntegral b * p * q)
                                   as' bs'' ps'' qs'')
    | n > 1  =
        let (pl, ql, bl, tl) = abpq as bs ps qs n1 m
            (pr, qr, br, tr) = abpq as bs ps qs m n2
        in (pl * pr, ql * qr, bl * br, fromIntegral br * qr * tl + fromIntegral bl * pl * tr)
    | otherwise = error "Non-expected case in binary splitting"
  where
    n = n2 - n1
    m = (n1 + n2 + 1) `div` 2

ones = repeat 1

-- To be changed to Stirling formula if that is faster
log2Factorials :: [Int]
log2Factorials = map integerLog2 . scanl1 (*) $ 1:[1..]

-- Straighforward Taylor summation

taylor :: Precision -> [Approx] -> [Integer] -> Approx
taylor res as qs =
  let res' = res + errorBits
      f r a q = limitAndBound res' $ a * recipA res' (fromIntegral q)
      g Bottom = False
      g (Approx m _ s) = abs m >= bit (s + res')
      bs = zipWith3 f (repeat res') as qs
      (cs,(d:_)) = span g bs
  in fudge (sum cs) d

{- Exponential computed by standard Taylor expansion after range reduction.
-}

-- Is faster for small approximations < ~2000 bits.
expA :: Precision -> Approx -> Approx
expA = expTaylorA

expBinarySplittingA :: Precision -> Approx -> Approx
expBinarySplittingA _ Bottom = Bottom
expBinarySplittingA res a@(Approx m e s) =
  let s' = s + integerLog2 m
      -- r' chosen so that a' below is smaller than 1/2
      r' = floor . sqrt . fromIntegral . max 5 $ res
      r = s' + r'
      -- a' is a scaled by 2^k so that 2^(-r') <= a' < 2^(-r'+1)
      a' = Approx m e (s-r)
      (Finite c) = min (significance a) (Finite res)
      (Just n) = findIndex (>= res+r) $ zipWith (+) log2Factorials [0,r'..]
      (p, q, b, t) = abpq ones
                          ones
                          (1:repeat a')
                          (1:[1..])
                          0
                          n
      nextTerm = a * p * recipA (res+r) (fromIntegral n * q)
      ss = iterate (boundErrorTerm . sqrA) $ fudge (t * recipA (res+r) (fromIntegral b*q)) nextTerm
  in ss !! r

expTaylorA :: Precision -> Approx -> Approx
expTaylorA _ Bottom = Bottom
expTaylorA res a@(Approx m e s) =
  let s' = s + integerLog2 m
      -- r' chosen so that a' below is smaller than 1/2
      r' = floor . sqrt . fromIntegral . max 5 $ res
      r = s' + r'
      -- a' is a scaled by 2^k so that 2^(-r') <= a' < 2^(-r'+1)
      a' = (Approx m e (s-r))
      t = taylor
            (res + r)
            (iterate (a'*) 1)
            (scanl1 (*) $ 1:[1..])
  in (!! r) . iterate (boundErrorTerm . sqrA) $ t
   
{- Logarithms computed by ln x = 2*atanh ((x-1)/(x+1)) after range reduction.
-}

-- Binary splitting is twice as fast as Taylor. AGM should be used over ~1000 bits.
logA :: Precision -> Approx -> Approx
logA = logBinarySplittingA

logBinarySplittingA :: Precision -> Approx -> Approx
logBinarySplittingA _ Bottom = Bottom
logBinarySplittingA res a@(Approx m e s) =
    if m <= e then Bottom -- only defined for strictly positive arguments
    else
        let r = s + integerLog2 (3*m) - 1
            a' = Approx m e (s-r)  -- a' is a scaled by a power of 2 so that 2/3 <= |a'| <= 4/3
            u = a' - 1
            v = a' + 1
            u2 = sqrA u
            v2 = sqrA v
            Finite res' = min (significance a) (Finite res)
            n = ceiling . (/2) $ fromIntegral (-res')/(log 0.2/log 2) - 1
            (p, q, b, t) = abpq (repeat 2)
                                [1,3..]
                                (u:repeat u2)
                                (v:repeat v2)
                                0
                                n
            nextTerm = recipA (res') 5 ^^ (2*n+1)
        in boundErrorTerm $ fudge (t * recipA res (fromIntegral b*q) + fromIntegral r * log2A (-res)) nextTerm

logTaylorA :: Precision -> Approx -> Approx
logTaylorA _ Bottom = Bottom
logTaylorA res a@(Approx m e s) =
    if m <= e then Bottom -- only defined for strictly positive arguments
    else
        let res' = res + errorBits
            r = s + integerLog2 (3*m) - 1
            a' = Approx m e (s-r)  -- a' is a scaled by a power of 2 so that 2/3 <= a' <= 4/3
            u = a' - 1
            v = a' + 1
            x = u * recipA (res') v  -- so |u/v| <= 1/5
            x2 = boundErrorTerm $ sqrA x
            t = taylor
                  res'
                  (iterate (x2*) x)
                  [1,3..]
        in boundErrorTerm $ 2 * t + fromIntegral r * log2A (-res')

sinA :: Precision -> Approx -> Approx
sinA _ Bottom = Bottom
sinA res a =
    let pi = piBorweinA res
        a1@(Approx m' e' s') = 4 * a * recipA res pi
        (k,m1) = m' `divMod` bit (-s')
        a2 = pi * fromDyadic (1:^(-2)) * (Approx m1 e' s')
    in case k `mod` 8 of
         0 -> sinInRangeA res a2
         1 -> cosInRangeA res (pi * fromDyadic (1:^(-2)) - a2)
         2 -> cosInRangeA res a2
         3 -> sinInRangeA res (pi * fromDyadic (1:^(-2)) - a2)
         4 -> - sinInRangeA res a2
         5 -> - cosInRangeA res (pi * fromDyadic (1:^(-2)) - a2)
         6 -> - cosInRangeA res a2
         7 -> - sinInRangeA res (pi * fromDyadic (1:^(-2)) - a2)

cosA :: Precision -> Approx -> Approx
cosA _ Bottom = Bottom
cosA res a =
    let pi = piBorweinA res
        a1@(Approx m' e' s') = 4 * a * recipA res pi
        (k,m1) = m' `divMod` bit (-s')
        a2 = pi * fromDyadic (1:^(-2)) * (Approx m1 e' s')
    in case k `mod` 8 of
         0 -> cosInRangeA res a2
         1 -> sinInRangeA res (pi * fromDyadic (1:^(-2)) - a2)
         2 -> - sinInRangeA res a2
         3 -> - cosInRangeA res (pi * fromDyadic (1:^(-2)) - a2)
         4 -> - cosInRangeA res a2
         5 -> - sinInRangeA res (pi * fromDyadic (1:^(-2)) - a2)
         6 -> sinInRangeA res a2
         7 -> cosInRangeA res (pi * fromDyadic (1:^(-2)) - a2)

atanA :: Precision -> Approx -> Approx
atanA _ Bottom = Bottom
atanA res a@(Approx m e s) =
  let rr x = x * recipA res (1 + sqrtA res (1 + sqrA x))
      a' = rr . rr . rr $ a -- range reduction so that |a'| < 1/4
      a2 = - sqrA a'
      Finite res' = min (significance a) (Finite res)
      n = (res' + 1) `div` 2
      (p, q, b, t) = abpq ones
                          [1,3..]
                          (a':repeat a2)
                          (repeat 1)
                          0
                          n
      nextTerm = Approx 1 0 (-2*n)
  in boundErrorTerm . (8*) $ fudge (t * recipA res (fromIntegral b*q)) nextTerm

swapSinCos :: Precision -> Approx -> Approx
swapSinCos res a = sqrtA res $ 1 - sqrA a

-- Computes sine if second argument is in the range [0,pi/4]
sinInRangeA :: Precision -> Approx -> Approx
sinInRangeA _ Bottom = Bottom
sinInRangeA res a =
    let n = res `div` 2        -- need to improve this estimate (is valid from res>=80)
        (p, q, b, t) = abpq ones
                            ones
                            (a:repeat (- sqrA a))
                            (1:[2*n*(2*n+1) | n <- [1..]] :: [Approx])
                            0
                            n
        nextTerm = fromDyadic (1:^(-res))
    in boundErrorTerm $ fudge (t * recipA res (fromIntegral b*q)) nextTerm

-- Computes cosine if second argument is in the range [0,pi/4]
cosInRangeA :: Precision -> Approx -> Approx
cosInRangeA _ Bottom = Bottom
cosInRangeA res a =
    let n = res `div` 2        -- need to improve this estimate (is valid from res>=80)
        (p, q, b, t) = abpq ones
                            ones
                            (1:repeat (- sqrA a))
                            (1:[2*n*(2*n-1) | n <- [1..]] :: [Approx])
                            0
                            n
        nextTerm = fromDyadic (1:^(-res))
    in boundErrorTerm $ fudge (t * recipA res (fromIntegral b*q)) nextTerm

piRaw :: [Approx]
piRaw = unfoldr f (1, (1, 1, 1, 13591409))
    where as = [13591409,13591409+545140134..]
          bs = ones
          ps = (1:[-(6*n-5)*(2*n-1)*(6*n-1) | n <- [1,2..]])
          qs = (1:[n^3*640320^2*26680 | n <- [1,2..]])
          f (i, (pl, ql, bl, tl)) = 
            let i2 = i*2
                (pr, qr, br, tr) = abpq as bs ps qs i i2
                n = 21+47*(i-1)
                x = fromIntegral tl * recipA n (fromIntegral (bl*ql))
                x1 = fudge x $ fromDyadic (1:^(-n))
                x2 = boundErrorTerm $ sqrtA n 1823176476672000 * recipA n x1
            in Just ( x2
                    , (i2, (pl * pr, ql * qr, bl * br, fromIntegral br * qr * tl + fromIntegral bl * pl * tr))
                    )


-- Second argument is noice to be added to first argument.
-- Used to allow for the error term when truncating a series.
fudge :: Approx -> Approx -> Approx
fudge (Approx m 0 s) (Approx m' e' s') =
  Approx (m `shift` (s - s')) (abs m' + e' + 1) s'
fudge (Approx m e s) (Approx m' e' s') =
  let m'' = 1 + (abs m' + e') `shift` (s' - s + 1)
  in Approx m (e+m'') s
fudge _ _  = Bottom

--

nonZeroCentred :: Approx -> Bool
nonZeroCentred Bottom = False
nonZeroCentred (Approx 0 _ _) = False
nonZeroCentred _ = True

piMachinA :: Precision -> Approx
piMachinA t = let (m:^s) = piMachinD (-t) in Approx m 1 s

piBorweinA :: Precision -> Approx
piBorweinA t = let (m:^s) = piBorweinD (-t) in Approx m 1 s

piAgmA t x = let t' = t - 10
                 a = 1
                 b = boundErrorTerm $ (2*x*recipA (-t') (x^2-1))^2
                 ss = agmA t a b
                 c = boundErrorTerm . (1-) . (*recipA (-t') (1-b^2)) . agm2 . agm1 $ ss
                 d = sqrtA (-t') (1+b)
                 b2 = b^2
                 b3 = b2*b
                 b4 = b2^2
                 l = boundErrorTerm $ (((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*c*d-1/(1+b)+(2+b2)/d) / ((2+(Approx 1 0 (-1))*b2+(Approx 9 0 (-5))*b4)*c+b2)
                 u = boundErrorTerm $ ((Approx 1 0 (-1))*b*c*d-1/(1+b)+(2+b2+(Approx 3 0 (-3))*b3+(Approx 9 0 (-3))*b4)/d) / ((2+(Approx 1 0 (-1))*b2)*c+b2+(Approx 9 0 (-3))*b4)
                 r = boundErrorTerm $ unionApprox l u
                 e = boundErrorTerm $ unionApprox
                      ((2+(Approx 1 0 (-1))*b2)*r-(Approx 1 0 (-1))*b*d)
                      ((2+(Approx 1 0 (-1))*b2+(Approx 9 0 (-5))*b4)*r-((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*d)
                 pi = boundErrorTerm $ unionApprox (2*(snd (last ss))*e) (2*(fst (last ss))*e)
             in pi
                
log2A :: Precision -> Approx
log2A t = let (m:^s) = ln2D t in Approx m 1 s

-- AGM

lnSuperSizeUnknownPi :: Precision -> Approx -> (Approx,Approx)
lnSuperSizeUnknownPi t x =
    let t' = t - 10
        a = 1
        b = boundErrorTerm $ (2*x*recipA (-t') (x^2-1))^2
        ss = agmA t a b
        (an,bn) = last ss
        c = boundErrorTerm . (1-) . (*recipA (-t') (1-b^2)) . agm2 . agm1 $ ss
        d = sqrtA (-t') (1+b)
        b2 = b^2
        b3 = b2*b
        b4 = b2^2
        l = boundErrorTerm $
             (((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*c*d-1/(1+b)+(2+b2)/d)
             / ((2+(Approx 1 0 (-1))*b2+(Approx 9 0 (-5))*b4)*c+b2)
        u = boundErrorTerm $
             ((Approx 1 0 (-1))*b*c*d-1/(1+b)+(2+b2+(Approx 3 0 (-3))*b3+(Approx 9 0 (-3))*b4)/d)
             / ((2+(Approx 1 0 (-1))*b2)*c+b2+(Approx 9 0 (-3))*b4)
        r = boundErrorTerm $ unionApprox l u
        e = boundErrorTerm $ unionApprox
             ((2+(Approx 1 0 (-1))*b2)*r-(Approx 1 0 (-1))*b*d)
             ((2+(Approx 1 0 (-1))*b2+(Approx 9 0 (-5))*b4)*r
              -((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*d)
        pi = boundErrorTerm $ unionApprox (2*bn*e) (2*an*e)
    in (r,pi) --[a,b,c,d,b2,b3,b4,l,u,r,e,pi]

lnSuperSizeKnownPi :: Precision -> Approx -> Approx -> Approx
lnSuperSizeKnownPi t pi x =
    let t' = t - 10
        a = 1
        b = boundErrorTerm $ (2*x*recipA (-t') (x^2-1))^2
        b2 = b^2
        b3 = b2*b
        b4 = b2^2
        b1sqrt = sqrtA (-t') (1+b)
        step (a,b) = (boundErrorTerm $ Approx 1 0 (-1) * (a+b)
                     ,boundErrorTerm $ sqrtA (-t') (a*b))
        close (a,b) = approximatedBy 0 $ a-b
        ((an,bn):_) = dropWhile (not . close) $ iterate step (a,b)
        i = boundErrorTerm $ unionApprox (pi*recipA (-t') (2*an)) (pi*recipA (-t') (2*bn))
        l = (i + ((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*b1sqrt)
            / (2 + (Approx 1 0 (-1))*b2 + (Approx 9 0 (-5))*b4)
        u = (i + (Approx 1 0 (-1))*b*b1sqrt) / (2 + (Approx 1 0 (-1))*b2)
    in boundErrorTerm $ unionApprox l u

lnLarge :: Precision -> Approx -> Approx
lnLarge t x =
    let (Finite k) = min (significance x) (Finite (-t))
        pi = piBorweinA t
        iL2 = integerLog2
        fI = fromIntegral
        n = max 0 . (1+) . (+(iL2 (fI k)-2)) . negate . iL2 . fI . iL2 . truncate $ toRational x
        (Approx m e s) = lnSuperSizeKnownPi t pi $ x^(2^n)
    in Approx m e (s-n)

lnSmall :: Precision -> Approx -> Approx
lnSmall t x@(Approx m _ s) =
    let (Finite t') = min (significance x) (Finite (-t))
        pi = piBorweinA t'
        iL2 = integerLog2
        fI = fromIntegral
        k = (-t) `div` 4 - iL2 m - s
        logx2k = lnSuperSizeKnownPi (-t') pi $ x * 2^k
        log2k = lnSuperSizeKnownPi (-t') pi $ 2^k
    in logx2k - log2k

logAgmA :: Precision -> Approx -> Approx
logAgmA t x
    | significance x < pure 5     = Bottom
    | 0 `approximatedBy` x        = Bottom
    | signum x == (-1)            = error "Trying to take logarithm of purely negative Approx."
    | lowerBound x > pure 2       = lnLarge t x
    | upperBound x < pure 3       = lnSmall t x
    | otherwise                   = error "Logic fault in logAgmA."

unionApprox :: Approx -> Approx -> Approx
unionApprox Bottom _ = Bottom
unionApprox _ Bottom = Bottom
unionApprox a b = fromEDI $ Interval.Interval (lowerBound a `min` lowerBound b) (upperBound a `max` upperBound b)

agmA :: Precision -> Approx -> Approx -> [(Approx,Approx)]
agmA t a b = let t' = t - 5
                 step (a,b) = (boundErrorTerm $ Approx 1 0 (-1) * (a+b), boundErrorTerm $ sqrtA (-t') (a*b))
                 close (a,b) = approximatedBy 0 $ a-b
             in (\(as, bs) -> as ++ take 1 bs) . break close $ iterate step (a,b)

sqDiff a b = boundErrorTerm $ a^2 - b^2

agm1 = zipWith (*) [Approx 1 0 i | i <- [-1,0..]] . map (uncurry sqDiff)

agm2 xs = sum (init xs) + unionApprox 0 (2 * last xs)

agmLn t x = let t' = t - 10
                a = 1
                b = boundErrorTerm $ (2*x*recipA (-t') (x^2-1))^2
                ss = agmA t a b
                (an,bn) = last ss
                c = boundErrorTerm . (1-) . (*recipA (-t') (1-b^2)) . agm2 . agm1 $ ss
                d = sqrtA (-t') (1+b)
                b2 = b^2
                b3 = b2*b
                b4 = b2^2
--                l = boundErrorTerm $ (((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*c*d-recipA t' (1+b)+(2+b2)*recipA t' d) * recipA t' ((2+(Approx 1 0 (-1))*b2+(Approx 9 0 (-5))*b4)*c+b2)
--                u = boundErrorTerm $ ((Approx 1 0 (-1))*b*c*d-recipA t' (1+b)+(2+b2+(Approx 3 0 (-3))*b3+(Approx 9 0 (-3))*b4)*recipA t' d) *recipA t' ((2+(Approx 1 0 (-1))*b2)*c+b2+(Approx 9 0 (-3))*b4)
                l = boundErrorTerm $ (((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*c*d-1/(1+b)+(2+b2)/d) / ((2+(Approx 1 0 (-1))*b2+(Approx 9 0 (-5))*b4)*c+b2)
                u = boundErrorTerm $ ((Approx 1 0 (-1))*b*c*d-1/(1+b)+(2+b2+(Approx 3 0 (-3))*b3+(Approx 9 0 (-3))*b4)/d) / ((2+(Approx 1 0 (-1))*b2)*c+b2+(Approx 9 0 (-3))*b4)
                r = boundErrorTerm $ unionApprox l u
                e = boundErrorTerm $ unionApprox
                      ((2+(Approx 1 0 (-1))*b2)*r-(Approx 1 0 (-1))*b*d)
                      ((2+(Approx 1 0 (-1))*b2+(Approx 9 0 (-5))*b4)*r-((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*d)
                pi = boundErrorTerm $ unionApprox (2*(snd (last ss))*e) (2*(fst (last ss))*e)
            in r --[a,b,c,d,b2,b3,b4,l,u,r,e,pi]
                
