--module Lll where
{-# LANGUAGE FlexibleContexts #-}

--import Control.Monad.State
import Data.CDAR
import Data.Foldable (toList)
--import Data.Int
import Data.Sequence (Seq (..))
import Data.Sequence as S
--import Data.Word
import Text.Parsec as P

type U = Int -- Word64
type I = Int -- Int64

data Cell
  = CU U
  | CI I
  | CAddr U
  | CZ Integer
  | CReal CR
  | CSeq (Seq Cell) deriving (Eq,Ord,Read)

instance Show Cell where
  show (CU u) = "CU " ++ show u
  show (CI u) = "CI " ++ show u
  show (CAddr u) = "CAddr " ++ show u
  show (CZ u) = "CZ " ++ show u
  show (CReal u) = "CR " ++ showCR 1 u
  show (CSeq u) = "CSeq " ++ show u

data PgmState = PgmState
  { pgm :: Seq Op
  , pc :: U
  , stack :: Seq Cell
  , rstack :: Seq U
  } deriving (Eq,Ord,Read)

instance Show PgmState where
  show (PgmState p cpc s r) = "PgmState " ++ show cpc ++ ' ' : show (S.index p cpc) ++ ' ' : show s ++ ' ' : show r

data Op
  = Dup U U | Pop U | Rot U U
  | Apush U | Scall U | Dcall | Ret | Jmp U | Jnz U
  | Ipush I | Ineg | Iadd | Imul | Idiv | Isgn
  | Zconv | Zneg | Zadd | Zmul | Zdiv | Zsgn | Zsh
  | Rconv | Rneg | Radd | Rmul | Rdiv | Rsh | Rch | Rlim U
  | Entc | Lvc U
  deriving (Eq,Ord,Read,Show)

data Result = Success PgmState | Error String | Result PgmState

--pident :: ParsecT () Char () String
--pident = many alphaNum

--plabel = pident <* char ':'

step :: Op -> PgmState -> Result
step op s@(PgmState _p cpc st rst) =
  case op of
    Dup n k | S.length st >= n -> Result $ s { stack = (S.take k . S.drop (n-k) $ st) >< st }
    Pop n | S.length st >= n -> Result $ s { stack = S.drop n $ st }
    Rot n k | S.length st >= n -> Result $ s { stack = let (as, ss) = S.splitAt n $ st
                                                           (bs, cs) = S.splitAt (n-k) as
                                                       in cs >< bs >< ss }
    Apush a -> Result $ s { stack = CAddr a <| st }
    Scall a -> Result $ s { pc = a, rstack = cpc <| rst }
    Dcall | S.length st >= 1 -> case st of
                                  CAddr a :<| ss -> Result $ s { pc = a, stack = ss, rstack = cpc <| rstack s }
                                  _ -> Main.Error "Dcall: Not an address"
    Ret -> case rstack s of
             (a :<| rs) -> Result $ s { pc = a, rstack = rs }
             S.Empty -> Success $ s
    Jmp a -> Result $ s { pc = a }
    Jnz a | S.length st >= 1 -> case st of
                                  (CI i :<| ss) -> Result $ if i /= 0
                                                            then s { pc = a, stack = ss }
                                                            else s { stack = ss }
                                  _ -> Main.Error "Jnz: Stack underflow"
    Ipush i -> Result $ s { stack = CI i <| st }
    Ineg -> iunary negate
    Iadd -> ibinary (+)
    Imul -> ibinary (*)
    Idiv -> ibinary div
    Isgn -> iunary signum
    Zconv -> case st of
               (CI a :<| ss) -> Result $ s { stack = CZ (fromIntegral a) <| ss }
               _ -> Main.Error "Zconv: Stack underflow or not an int."
    Zneg -> zunary negate
    Zadd -> zbinary (+)
    Zmul -> zbinary (*)
    Zdiv -> zbinary div
    Zsgn -> zunary signum
    Zsh -> case st of
             (CI a :<| CZ b :<| ss) -> Result $ s { stack = CZ (scale b a) <| ss }
             _ -> Main.Error "Zsh: Stack underflow or not correct types."
    Rconv -> case st of
               (CZ a :<| ss) -> Result $ s { stack = CReal (fromIntegral a) <| ss }
               _ -> Main.Error "Rconv: Stack underflow or not an integer."
    Rneg -> runary negate
    Radd -> rbinary (+)
    Rmul -> rbinary (*)
    Rdiv -> rbinary (/)
    Rsh -> case st of
             (CI a :<| CReal b :<| ss) -> Result $ s { stack = CReal (scale b a) <| ss }
             _ -> Main.Error "Rsh: Stack underflow or not correct types."
    Rch -> case st of
             (CI n :<| ss) -> let (cs, ss') = S.splitAt n ss
                                  (Select i) = selectCR (map (\(CReal x) -> x) (toList cs))
                              in Result $ s { stack = CI i <| ss' }
             _ -> Main.Error "Rsh:"
    Rlim a -> let f s' n = last . interpret $ s' { pc = a, stack = CI n <| st, rstack = S.Empty }
                  g n = case stack $ f s n of
                          CReal x :<| _ -> x
                          _s -> error $ "Rlim: Not correctly called.\n" ++ show _s
                  y = limCR g
              in Result $ s { stack = CReal y <| st }
    
    Entc -> Result s
    Lvc _ -> Result s
    _ -> Main.Error "Unknown op code"
  where
    iunary f = case st of
                 (CI a :<| ss) -> Result $ s { stack = CI (f a) <| ss }
                 _ -> Main.Error "Iunary: Stack underflow or not an int."
    ibinary f = case st of
                  (CI a :<| CI b :<| ss) -> Result $ s { stack = CI (f b a) <| ss }
                  _ -> Main.Error "Ibinary: Stack underflow or not an int."
    zunary f = case st of
                 (CZ a :<| ss) -> Result $ s { stack = CZ (f a) <| ss }
                 _ -> Main.Error "Zunary: Stack underflow or not an integer."
    zbinary f = case st of
                  (CZ a :<| CZ b :<| ss) -> Result $ s { stack = CZ (f b a) <| ss }
                  _ -> Main.Error "Zbinary: Stack underflow or not an integer."
    runary f = case st of
                 (CReal a :<| ss) -> Result $ s { stack = CReal (f a) <| ss }
                 _ -> Main.Error "Runary: Stack underflow or not a real."
    rbinary f = case st of
                  (CReal a :<| CReal b :<| ss) -> Result $ s { stack = CReal (f b a) <| ss }
                  _ -> Main.Error ("Rbinary: Stack underflow or not a real.\n" ++ show s)

interpret :: PgmState -> [PgmState]
interpret t =
  let cpc = pc t
      op = index (pgm t) cpc
      s = t { pc = cpc+1 }
  in case step op s of
    (Result s') -> t : interpret s'
    (Success s') -> [s']
    (Main.Error e) -> error e
    -- _ -> error ""

main :: IO ()
main = test boundLim 73 (CReal (0.001) <| S.Empty)

test :: Seq Op -> Int -> Seq Cell -> IO ()
test p initpc s = do
  let r = interpret (PgmState p initpc s S.Empty)
  mapM_ (putStrLn . show) r
  putStrLn . show . stack . last $ r

zero :: Seq Op
zero = fromList
  [ Rlim 2   -- limit of 2^(-n)
  , Ret      -- end
  , Ineg     -- p -> -p
  , Ipush 1  -- -p 1
  , Zconv
  , Rconv
  , Rot 2 1  -- 1 -p
  , Rsh      -- 2^-p
  , Ret      -- return from limit
  ]

-- lim p => case x < 2^p => -x || x > -2^p => x end
-- cases equiv:  2^p-x > 0     || 2^p+x > 0
-- abs_apx:        # x p
boundLim :: Seq Op
boundLim = S.fromList
  [ Ipush 1       -- x p 1
  , Zconv         -- x p 1
  , Rconv         -- x p 1
  , Rot 2 1       -- x 1 p
  , Rsh           -- x 2^p
  , Dup 2 2  -- x 2^p x 2^p
  , Rot 2 1  -- x 2^p 2^p x
  , Rneg    -- x 2^p 2^p -x
  , Radd    -- x 2^p 2^p-x
  , Rot 2 1  -- x 2^p-x 2^p
  , Dup 3 1  -- x 2^p-x 2^p x
  , Radd    -- x 2^p-x 2^p+x
  , Ipush 2  -- x 2^p-x 2^p+x 2
  , Rch    -- x k
-- k = 0 => 2^p-x < 0 and 2^p+x < 0 -- impossible
-- k = 1 => 2^p-x > 0               -- p-approximation is -x = (2k-3)*x
-- k = 2 => 2^p+x > 0               -- p-approximation is  x = (2k-3)*x
  , Ipush 2  -- x k 2
  , Imul    -- x 2k
  , Ipush (-1)  -- x 2k -3
  , Iadd    -- x 2k-3
  , Zconv    -- x 2k-3
  , Rconv    -- x 2k-3
  , Dup 2 1  -- x 2k-3 x
  , Rmul    -- x (2k-3)*x
  , Ret

-- 23 abs:    -- x
  , Rlim 0  -- x |x|
  , Rot 2 1  -- |x| x
  , Pop 1    -- |x|
  , Ret

--           case |x| > 2^(p-1) => 0 || |x| < 2^p => 1 end
-- cases equiv:   |x|-2^(p-1) > 0    || 2^p-|x| > 0
--               -- stack                      types
-- 27 bounded:        -- x p                        [R Z]
  , Rot 2 1       -- p x
  , Scall 23  -- p |x|
  , Rot 2 1  -- |x| p
  , Ipush 1       -- |x| p 1
  , Zconv         -- |x| p 1
  , Rconv         -- |x| p 1
  , Rot 2 1       -- |x| 1 p
  , Dup 2 2       -- |x| 1 p 1 p
  , Ipush (-1)      -- |x| 1 p 1 p -1
  , Iadd          -- |x| 1 p 1 p-1
  , Rsh           -- |x| 1 p 2^(p-1)
  , Dup 4 1       -- |x| 1 p 2^(p-1) |x|
  , Rot 2 1       -- |x| 1 p |x| 2^(p-1)
  , Rneg          -- |x| 1 p |x| -2^(p-1)
  , Radd          -- |x| 1 p |x|-2^(p-1)
  , Rot 4 3  -- |x|-2^(p-1) |x| 1 p
  , Rsh    -- |x|-2^(p-1) |x| 2^p
  , Rot 2 1  -- |x|-2^(p-1) 2^p |x|
  , Rneg    -- |x|-2^(p-1) 2^p -|x|
  , Radd    -- |x|-2^(p-1) 2^p-|x|
  , Ipush 2  -- |x|-2^(p-1) 2^p-|x| 2
  , Rch    -- k
  -- k = 0 => |x|-2^(p-1) < 0 and 2^p-|x| < 0
  -- k = 1 => |x|-2^(p-1) > 0                 -- bounded x p = 0 = k-1
  -- k = 2 =>                     2^p-|x| > 0 -- bounded x p = 1 = k-1
  , Ipush (-1)  -- k -1
  , Iadd    -- k-1
  , Ret

-- 52 zero_apx:  -- p
  , Ineg
  , Ipush 1  -- p 1
  , Zconv    -- p 1
  , Rconv    -- p 1
  , Dup 2 1  -- p 1 p
  , Rsh    -- p 2^p
  , Rot 2 1  -- 2^p p
  , Dup 1 1  -- 2^p p p
  , Ipush 2  -- 2^p p p 2
  , Idiv    -- 2^p p q
  , Ipush (-2)  -- 2^p p q -2
  , Imul    -- 2^p p -2q
  , Iadd    -- 2^p p-2q
  , Ipush 2  -- 2^p p-2q 2
  , Imul    -- 2^p 2(p-2q)
  , Ipush 1  -- 2^p 2(p-2q) 1
  , Iadd    -- 2^p 2(p-2q)+1
  , Zconv    -- 2^p 2(p-2q)+1
  , Rconv    -- 2^p 2(p-2q)+1
  , Rmul    -- +/-2^p
  , Ret

-- 73 main:            -- (args)
  , Pop 1          --
  , Ipush 1        -- 1
  , Entc           -- 1 p
  , Rlim 52 -- 1 p 0
  , Ipush (-2000)    -- 1 p 0 2
  , Scall 27  -- 1 p z
  , Lvc 1          -- 1 z
  , Rot 2 1        -- z 1
  , Pop 1          -- z
  , Ret
  , Ret
  ]
