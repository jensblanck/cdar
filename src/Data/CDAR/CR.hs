{-# LANGUAGE GADTs #-}
module Data.CDAR.CR (module Data.CDAR.CR) where

--import Control.Applicative
import qualified Data.CDAR.Approx as A
import Data.CDAR.Approx hiding (toDouble)
import Data.CDAR.Dyadic
--import Data.CDAR.Extended
import Data.Map (Map)
import qualified Data.Map as M

-- adding prototype reals

type Var = String

data CR where
  CRInteger :: Integer -> CR
  CRRational :: Rational -> CR
  CRDyadic :: Dyadic -> CR
  CRAdd :: CR -> CR -> CR
  CRMul :: CR -> CR -> CR
  CRNeg :: CR -> CR
  CRAbs :: CR -> CR
  CRSignum :: CR -> CR
  CRRecip :: CR -> CR
  CRLet :: Var -> CR -> CR -> CR
  CRVar :: Var -> CR
  CRPoly :: [CR] -> CR -> CR
        deriving (Show)
-- add polynomials in one and several variables
-- and rational functions
-- and trancendental functions

type Resources = [Int]

startLimit :: Int
startLimit = 80

bumpLimit :: Int -> Int
bumpLimit n = n * 3 `div` 2

resources :: Resources
resources = iterate bumpLimit startLimit

instance Eq CR where
    _ == _ = False

--instance Show CR where
--    show = show . evalCR 40

instance Num CR where
    x + y = CRAdd x y
    x * y = CRMul x y
    negate x = CRNeg x
    abs x = CRAbs x
    signum x = CRSignum x
    fromInteger n = CRInteger n

instance Fractional CR where
    recip x = CRRecip x
    fromRational x = CRRational x

instance Ord CR where
    compare = undefined

instance Real CR where
    toRational = toRational . evalCR 40

ok :: Int -> Approx -> Approx
ok d a = if precision a > fromIntegral d then a else Bottom

ok' :: Int -> Approx -> Bool
ok' d a = precision a > fromIntegral d

toDouble :: CR -> Double
toDouble = A.toDouble . evalCR (54+errorBits)

evalCR :: Int -> CR -> Approx
evalCR k x = head . dropWhile (not . ok' k) $ map (flip eval' x) resources

eval' :: Int -> CR -> Approx
eval' = eval M.empty

eval :: Map String Approx -> Int -> CR -> Approx
--eval x y = trace (show y) $ eval' x y
eval _ _ (CRInteger n) = fromInteger n
eval _ l (CRRational r) = toApprox l r
eval _ _ (CRDyadic (a:^s)) = Approx a 0 s
eval t l (CRAdd x y) = limitAndBound l . ok 3 $ (eval t l x) + (eval t l y)
eval t l (CRMul x y) = limitAndBound l . ok 3 $ (eval t l x) * (eval t l y)
eval t l (CRNeg x) = negate $ eval t l x
eval t l (CRAbs x) = abs $ eval t l x
eval t l (CRSignum x) = signum $ eval t l x
eval t l (CRRecip x) =
    limitAndBound l . ok 3 $
    let a = eval t l x
    in if exact a
       then recipDyadic (centre a) l
       else recip a
eval t _ (CRVar x) = maybe Bottom id $ M.lookup x t
eval t l (CRLet s x y) = let z = eval t l x
                             t' = M.insert s z t
                         in eval t' l y
eval t l (CRPoly as x) = limitAndBound l . ok 3 $ poly (map (eval t l) as) (eval t l x)
-- eval _ _ _ = error "Unhandled case in evaluation of real"

-- foo :: CR -> (Map String Approx, Int) -> Approx
-- foo (CRInteger n) = const $ fromInteger n
-- foo (CRRational r) = \(_, l) -> toApprox l r
-- foo (CRDyadic (a:^s)) = const $ Approx a 0 s
-- foo (CRAdd x y) = \e@(_,l) -> limitAndBound l . ok 3 $ (+) <$> foo x <*> foo y $ e
-- foo (CRMul x y) = \e@(_,l) -> limitAndBound l . ok 3 $ (*) <$> foo x <*> foo y $ e
-- foo (CRNeg x) = negate <$> foo x
-- foo (CRAbs x) = abs <$> foo x
-- foo (CRSignum x) = signum <$> foo x
-- foo (CRRecip x) = \e@(_,l) -> (\a -> if exact a
--                                      then recipDyadic (centre a) l
--                                      else limitAndBound l . ok 3 . recip a) <$> foo x $ e
-- foo _ = \_ -> Bottom
