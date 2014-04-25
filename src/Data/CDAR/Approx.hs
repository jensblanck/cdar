--{-# OPTIONS_GHC -fglasgow-exts #-}
module Data.CDAR.Approx (Approx(..)
                        ,errorBits
                        ,errorBound
                        ,showA
                        ,testShowA
--                        ,valid
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
                        ,recipDyadic
                        ,modA
                        ,divModA
--                        ,normalise
--                        ,roundApprox
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
                        ,bincoffs
                        ,powers
                        ,sqrtA
                        ,sqrtD
                        ,shiftD
                        ,nonZero
                        ,piMachinA
                        ,piBorweinA
                        ,piAgmA
                        ,ln2A
                        ,agmLnA
                        ,agmLn) where

import Data.Ratio
import Data.Bits
import Data.CDAR.Dyadic hiding (normalise)
import qualified Data.CDAR.Interval as Interval
import Data.CDAR.Extended
import Data.CDAR.IntegerLog
import Data.CDAR.POrd
import Data.List (unfoldr)
import Data.Char (intToDigit)
import Control.Applicative
import Control.Exception
import Control.DeepSeq

type EDI = Interval.Interval (Extended Dyadic)

-- Centred dyadic approximations

data Approx = Approx Integer Integer Int
	    | Bottom
	      deriving (Read,Show)

instance NFData Approx where
    rnf Bottom = ()
    rnf (Approx m e s) = rnf (m,e,s) `seq` ()

errorBits :: Int
errorBits = 10

errorBound :: Integer
errorBound = 2^errorBits

showA :: Approx -> String
showA Bottom = "_|_"
showA (Approx m e s)
    | e == 0    = sign ++ showExactA b i f
    | am < e    = "+-" ++ showNearZeroA b i' f'
    | otherwise = sign ++ showInexactA b i f e
    where b = bit (max 0 (-s))
          am = abs m
          i = shift am s
          f = am .&. (b-1)
          i' = shift (am+e) s
          f' = (am+e) .&. (b-1)
          sign = if m < 0 then "-" else ""

showExactA :: Integer -> Integer -> Integer -> String
showExactA b i f = 
    let g i' = let (q,r) = quotRem i' 10
               in if i' == 0 then Nothing
                  else Just (intToDigit (fromIntegral r), q)
        ip = reverse (unfoldr g i)
        h f' = let (q,r) = quotRem (10*f') b
               in if f' == 0 then Nothing
                  else Just (intToDigit (fromIntegral q), r)
        fp = unfoldr h f
    in (if null ip then "0" else ip)
           ++ (if null fp then "" else ".")
           ++ fp

showNearZeroA :: Integer -> Integer -> Integer -> String
showNearZeroA b i f =
    let s = showExactA b i f
        t = takeWhile (flip elem "0.~") s
        u = takeWhile (/= '.') s
    in if null t
       then replicate (length u) '~'
       else t ++ "~"

showInexactA :: Integer -> Integer -> Integer -> Integer -> String
showInexactA b i f e =
    let g (n,b',f') = let (q,r) = quotRem n 10
                          z = (q, 10*b', r*b'+f')
                      in if n == 0 then Nothing
                         else if e+f' <= b'
                              then Just (intToDigit (fromIntegral r), z)
                              else if e <= min f' b'
                                   then Just (intToDigit ((fromIntegral r + 1) `rem` 10), z)
                                   else Just ('~', z)
        intRev = unfoldr g (i,b,f)
        noFrac = case intRev of
                   [] -> False
                   (x:_) -> x == '~'
        int = if null intRev then "0" else reverse intRev
        h (f',err) = let (q,r) = quotRem (10*f') b
                         err' = 10*err
                         z = (r, err')
                     in if err' + r <= b
                        then Just (intToDigit (fromIntegral q), z)
                        else if err' <= min r b
                             then Just (intToDigit ((fromIntegral q + 1) `rem` 10), z)
                             else Nothing
        frac = unfoldr h (f,e)
    in int ++ if noFrac
              then ""
              else "." ++ frac ++ "~"

-- can be used like this: sequence_ . map (test) $ [piBorweinA (-s) | s <- [0..53]]

testShowA :: Approx -> IO ()
testShowA a = let (Finite l) = lowerBound a
                  (Finite u) = upperBound a
              in do print (fromRational $ toRational l :: Double)
                    putStrLn $ showA a
                    print (fromRational $ toRational u :: Double)
                    putStrLn "---"

{-
valid :: Approx -> Bool
valid Bottom = True
valid (Approx _ e _) = e >= 0
-}

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
        | am >= e && an >= f && a > 0		= Approx (a+d) (ab+ac) u
        | am >= e && an >= f && a < 0		= Approx (a-d) (ab+ac) u
        | am < e && n >= f			= Approx (a+b) (ac+d) u
        | am < e && -n >= f			= Approx (a-b) (ac+d) u
        | m >= e && an < f			= Approx (a+c) (ab+d) u
        | -m >= e && an < f			= Approx (a-c) (ab+d) u
        | a == 0				= Approx (0) (ab+ac+d) u
        | am < e && an < f && a > 0 && ab > ac	= Approx (a+ac) (ab+d) u
        | am < e && an < f && a > 0 && ab <= ac	= Approx (a+ab) (ac+d) u
        | am < e && an < f && a < 0 && ab > ac	= Approx (a-ac) (ab+d) u
        | am < e && an < f && a < 0 && ab <= ac	= Approx (a-ab) (ac+d) u
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

toApprox :: Int -> Rational -> Approx
toApprox t r = Approx (2 * round (r*2^^t)) 1 (-t - 1)

-- Not a proper Fractional type as Approx are intervals
instance Fractional Approx where
    fromRational = toApprox 31    -- Approximately 10 digits
    recip Bottom = Bottom
    recip (Approx m e s)
	| (abs m) > e =	let d = m*m-e*e
			    t = 2 * (integerLog2 m + errorBits)
			  in Approx
				 (round (unsafeShiftL m t%(d)))
				 (ceiling (1%2 + unsafeShiftL e t%(d)))
				 (-s-t)
        | otherwise   = Bottom

recipA :: Int -> Approx -> Approx
recipA _ Bottom = Bottom
recipA t (Approx m e s)
    | e == 0      = let s' = t - errorBits - 5 - integerLog2 m
                    in boundErrorTerm $ Approx
                           (round (unsafeShiftL 1 (-s') % m))
                           1
                           s'
    | (abs m) > e = let d = m*m-e*e
			s' = 2 * (integerLog2 m + errorBits)
		    in boundErrorTerm $ Approx
                           (round (unsafeShiftL m s'%(d)))
                           (ceiling (1%2 + unsafeShiftL e s'%(d)))
			   (-s-s')
    | otherwise   = Bottom

recipDyadic :: Dyadic -> Int -> Approx
recipDyadic (m:^s) l = Approx (round (bit t%m)) 1 (-s-t)
    where t = l + integerLog2 (abs m) + errorBits

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

-- Not a proper Ord type as Approx are intervals
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

{-
instance PartialOrd Approx where
    partialCompare a@(Approx m e s) b@(Approx n f t)
        | exact a && exact b                  = Just $ compare (m:^s) (n:^t)
        | abs ((m:^s)-(n:^t)) > (e:^s)+(f:^t) = Just $ compare (m:^s) (n:^t)
        | otherwise                           = Nothing
    partialCompare _ _ = Nothing
-}

{-
normalise :: Approx -> Approx
normalise (Approx 0 0 _) = Approx 0 0 0
normalise a@(Approx m e s)
    | even m && even e = normalise $ Approx (div m 2) (div e 2) (s+1)
    | otherwise = a
normalise _ = Bottom
-}

instance Real Approx where
    toRational (Approx m e s) = approxRational
				  (toRational (m:^s))
				  (toRational (e:^s))
    toRational _ = undefined

toDouble :: Approx -> Double
toDouble = fromRational . toRational . centre

toDouble2 :: Approx -> Interval.Interval Double
toDouble2 = fmap (fromRational . toRational) . toEDI

{-
roundApprox :: Approx -> Int -> Approx
roundApprox = roundApprox' 0

roundApprox' :: Int -> Approx -> Int -> Approx
roundApprox' i a@(Approx m e s) j = 
    let q = 1 + integerLog2 e
	k = (q - j + i) `max` 0
	x = bit k
	t = s + k
	(n', r') = m `divMod` x
	(n, r) = if r' > (x `div` 2) then (n'+1, abs (r'-x))
		 else (n', r')
	e' = - (unsafeShiftR (-e-r) k)
      in if integerLog2 e' < j then Approx n e' t
	 else roundApprox' (i+1) a j
roundApprox' _ _ _ = Bottom
-}

precision :: Approx -> Extended Int
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

limitSize :: Int -> Approx -> Approx
limitSize _ Bottom = Bottom
limitSize l a@(Approx m e s)
    | k > 0     = Approx
		  ((if testBit m (k-1) then (+1) else id) (unsafeShiftR m k))
		  (1 + (unsafeShiftR (e + bit (k-1)) k))
		  (-l)
    | otherwise = a
    where k = (-s)-l

-- 31 gives approximately 10 decimal digits of precision

checkPrecisionLeft :: Approx -> Approx
checkPrecisionLeft a
	| precision a > 31 = a
	| otherwise        = throw $ LossOfPrecision

limitAndBound :: Int -> Approx -> Approx
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

bincoffs :: (Num a) => [[a]]
bincoffs =
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
    in zipWith g (iterate (+s) 0) $ map (sumAlt . f) bincoffs
powers _ = repeat Bottom

sqrtA :: Int -> Approx -> Approx
sqrtA _ Bottom = Bottom
sqrtA k a@(Approx m e s)
    | -m > e    = error "Attempting sqrt of Approx containing only negative numbers."
    | m < e     = Bottom
    | e == 0    = let (n:^t) = shiftD k $ sqrtD (k-2) (m:^s)
                  in Approx n 1 t
    | m == e    = let (n:^t) = sqrtD (s `quot` 2 -errorBits) ((m+e):^s)
                      n' = (n+2) `quot` 2
                  in Approx n' n' t
    | otherwise = let (Finite p) = significance a
                      s' = s `quot` 2 - p - errorBits
                      l@(n:^t) = sqrtD s' ((m-e):^s)
                      (n':^t') = sqrtD' s' ((m+e):^s) l
                  in fromEDI $ Interval.Interval (Finite ((n-1):^t)) (Finite ((n'+1):^t'))

--checks for insignificant approximations rather than actual non-zero check
nonZero :: Approx -> Bool
nonZero Bottom = False
nonZero (Approx 0 _ _) = False
nonZero _ = True

piMachinA :: Int -> Approx
piMachinA t = let (m:^s) = piMachinD t in Approx m 1 s

piBorweinA :: Int -> Approx
piBorweinA t = let (m:^s) = piBorweinD t in Approx m 1 s

piAgmA t x = let t' = t - 10
                 a = 1
                 b = boundErrorTerm $ (2*x*recipA t' (x^2-1))^2
                 ss = agmA t a b
                 c = boundErrorTerm . (1-) . (*recipA t' (1-b^2)) . agm2 . agm1 $ ss
                 d = sqrtA t' (1+b)
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
                
ln2A :: Int -> Approx
ln2A t = let (m:^s) = ln2D t in Approx m 1 s

-- AGM

lnSuperSizeUnknownPi :: Int -> Approx -> (Approx,Approx)
lnSuperSizeUnknownPi t x =
    let t' = t - 10
        a = 1
        b = boundErrorTerm $ (2*x*recipA t' (x^2-1))^2
        ss = agmA t a b
        (an,bn) = last ss
        c = boundErrorTerm . (1-) . (*recipA t' (1-b^2)) . agm2 . agm1 $ ss
        d = sqrtA t' (1+b)
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

lnSuperSizeKnownPi :: Int -> Approx -> Approx -> Approx
lnSuperSizeKnownPi t pi x =
    let t' = t - 10
        a = 1
        b = boundErrorTerm $ (2*x*recipA t' (x^2-1))^2
        b2 = b^2
        b3 = b2*b
        b4 = b2^2
        b1sqrt = sqrtA t' (1+b)
        step (a,b) = (boundErrorTerm $ Approx 1 0 (-1) * (a+b)
                     ,boundErrorTerm $ sqrtA t' (a*b))
        close (a,b) = approximatedBy 0 $ a-b
        ((an,bn):_) = dropWhile (not . close) $ iterate step (a,b)
        i = boundErrorTerm $ unionApprox (pi*recipA t' (2*an)) (pi*recipA t' (2*bn))
        l = (i + ((Approx 1 0 (-1))*b-(Approx 3 0 (-4))*b2+(Approx 9 0 (-5))*b3)*b1sqrt)
            / (2 + (Approx 1 0 (-1))*b2 + (Approx 9 0 (-5))*b4)
        u = (i + (Approx 1 0 (-1))*b*b1sqrt) / (2 + (Approx 1 0 (-1))*b2)
    in boundErrorTerm $ unionApprox l u

lnLarge :: Int -> Approx -> Approx
lnLarge t x =
    let (Finite k) = min (significance x) (Finite (-t))
        pi = piBorweinA t
        iL2 = integerLog2
        fI = fromIntegral
        n = max 0 . (1+) . (+(iL2 (fI k)-2)) . negate . iL2 . fI . iL2 . truncate $ toRational x
        (Approx m e s) = lnSuperSizeKnownPi t pi $ x^(2^n)
    in Approx m e (s-n)

lnSmall :: Int -> Approx -> Approx
lnSmall t x@(Approx m _ s) =
    let (Finite t') = min (significance x) (Finite (-t))
        pi = piBorweinA t'
        iL2 = integerLog2
        fI = fromIntegral
        k = (-t) `div` 4 - iL2 m - s
        logx2k = lnSuperSizeKnownPi (-t') pi $ x * 2^k
        log2k = lnSuperSizeKnownPi (-t') pi $ 2^k
    in logx2k - log2k

agmLnA :: Int -> Approx -> Approx
agmLnA t x
    | significance x < pure 5     = Bottom
    | 0 `approximatedBy` x        = Bottom
    | signum x == (-1)            = error "Trying to take logarithm of purely negative Approx."
    | lowerBound x > pure 2       = lnLarge t x
    | upperBound x < pure 3       = lnSmall t x
    | otherwise                   = error "Logic fault in agmLnA."

unionApprox :: Approx -> Approx -> Approx
unionApprox Bottom _ = Bottom
unionApprox _ Bottom = Bottom
unionApprox a b = fromEDI $ Interval.Interval (lowerBound a `min` lowerBound b) (upperBound a `max` upperBound b)

agmA :: Int -> Approx -> Approx -> [(Approx,Approx)]
agmA t a b = let t' = t - 5
                 step (a,b) = (boundErrorTerm $ Approx 1 0 (-1) * (a+b), boundErrorTerm $ sqrtA t' (a*b))
                 close (a,b) = approximatedBy 0 $ a-b
             in (\(as, bs) -> as ++ take 1 bs) . break close $ iterate step (a,b)

sqDiff a b = boundErrorTerm $ a^2 - b^2

agm1 = zipWith (*) [Approx 1 0 i | i <- [-1,0..]] . map (uncurry sqDiff)

agm2 xs = sum (init xs) + unionApprox 0 (2 * last xs)

agmLn t x = let t' = t - 10
                a = 1
                b = boundErrorTerm $ (2*x*recipA t' (x^2-1))^2
                ss = agmA t a b
                (an,bn) = last ss
                c = boundErrorTerm . (1-) . (*recipA t' (1-b^2)) . agm2 . agm1 $ ss
                d = sqrtA t' (1+b)
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
                


-- Auxiliary. May be removed.
{-
showDHex :: Dyadic -> String
showDHex (m:^s) = let k = max 0 $ integerLog2 m - 7
                  in showHex (unsafeShiftR m k) ":^" ++ show (s+k)

getNmsb :: Int -> Integer -> Int
getNmsb i n = let k = max 0 $ integerLog2 n - (i-1)
              in fromIntegral $ unsafeShiftR n k

getM :: Approx -> Integer
getM (Approx m _ _) = m

class ShowBinary a where
    showBinary :: a -> String

showBinaryInteger :: Integer -> String
showBinaryInteger 0 = "0"
showBinaryInteger n = reverse $ f n
    where f 0 = []
          f n = intToDigit (fromIntegral (n `rem` 2)) : f (n `quot` 2)

instance ShowBinary Int where
    showBinary n = if n >= 0
                   then showBinaryInteger $ fromIntegral n
                   else showBinaryInteger $ fromIntegral (-n)

instance ShowBinary Integer where
    showBinary n = if n >= 0
                   then showBinaryInteger n
                   else showBinaryInteger (-n)

instance ShowBinary Dyadic where
    showBinary (m:^s) = showBinary m ++ ":^" ++ show s

instance ShowBinary Approx where
    showBinary (Approx m e s) = "Approx " ++ showBinary m ++ " " ++ showBinary e ++ " " ++ show s
-}
