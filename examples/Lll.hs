--module Lll where
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Data.CDAR
import Data.Foldable (toList)
import Data.Int
import Data.Sequence (Seq (..))
import Data.Sequence as S
import Data.Word

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
  show (CReal _) = "CR"
  show (CSeq u) = "CSeq " ++ show u

data PgmState = PgmState
  { pc :: U
  , stack :: Seq Cell
  , rstack :: Seq U
  , pstack :: Seq U
  } deriving (Eq,Ord,Read,Show)

data Op
  = Dup U U
  | Pop U
  | Rot U U
  | Apush U
  | Scall U
  | Dcall
  | Ret
  | Jmp U
  | Jnz U
  | Ipush I
  | Ineg
  | Iadd
  | Imul
  | Idiv
  | Isgn
  | Zconv
  | Zneg
  | Zadd
  | Zmul
  | Zdiv
  | Zsgn
  | Zsh
  | Rconv
  | Rneg
  | Radd
  | Rmul
  | Rdiv
  | Rsh
  | Rch
  | Entc
  | Lvc U
  deriving (Eq,Ord,Read,Show)

step :: Seq Op -> State PgmState Bool
step prg = do
  t <- get
  let cpc = pc t
  if (cpc >= S.length prg)
    then return False
    else do
      let op = index prg cpc
          s = t { pc = cpc+1 }
          s' = case op of
                 Dup n k -> s { stack = (S.take k . S.drop (n-k) $ stack s) >< stack s }
                 Pop n -> s { stack = S.drop n $ stack s }
                 Rot n k -> s { stack = let (as, ss) = S.splitAt n $ stack s
                                            (bs, cs) = S.splitAt (n-k) as
                                        in cs >< bs >< ss }
                 Apush a -> s { stack = CAddr a <| stack s }
                 Scall a -> s { pc = a, rstack = pc s <| rstack s }
                 Dcall -> let (CAddr a :<| ss) = stack s
                          in s { pc = a, stack = ss, rstack = pc s <| rstack s }
                 Ret -> let (a :<| rs) = rstack s
                        in s { pc = a, rstack = rs }
                 Jmp a -> s { pc = a }
                 Jnz a -> let (CI i :<| ss) = stack s
                          in if i /= 0 then s { pc = a, stack = ss } else s { stack = ss }
                 Ipush i -> s { stack = CI i <| stack s }
                 Ineg -> let (CI a :<| ss) = stack s
                         in s { stack = CI (-a) <| ss }
                 Iadd -> let (CI a :<| CI b :<| ss) = stack s
                         in s { stack = CI (b+a) <| ss }
                 Imul -> let (CI a :<| CI b :<| ss) = stack s
                         in s { stack = CI (b*a) <| ss }
                 Idiv -> let (CI a :<| CI b :<| ss) = stack s
                         in s { stack = CI (b `div` a) <| ss }
                 Isgn -> let (CI a :<| ss) = stack s
                         in s { stack = CI (signum a) <| ss }
                 Zconv -> let (CI a :<| ss) = stack s
                          in s { stack = CZ (fromIntegral a) <| ss }
                 Zneg -> let (CZ a :<| ss) = stack s
                         in s { stack = CZ (-a) <| ss }
                 Zadd -> let (CZ a :<| CZ b :<| ss) = stack s
                         in s { stack = CZ (b+a) <| ss }
                 Zmul -> let (CZ a :<| CZ b :<| ss) = stack s
                         in s { stack = CZ (b*a) <| ss }
                 Zdiv -> let (CZ a :<| CZ b :<| ss) = stack s
                         in s { stack = CZ (b `div` a) <| ss }
                 Zsgn -> let (CZ a :<| ss) = stack s
                         in s { stack = CZ (signum a) <| ss }
                 Zsh -> let (CI a :<| CZ b :<| ss) = stack s
                        in s { stack = CZ (scale b a) <| ss }
                 Rconv -> let (CZ a :<| ss) = stack s
                          in s { stack = CReal (fromIntegral a) <| ss }
                 Rneg -> let (CReal a :<| ss) = stack s
                         in s { stack = CReal (-a) <| ss }
                 Radd -> let (CReal a :<| CReal b :<| ss) = stack s
                         in s { stack = CReal (b+a) <| ss }
                 Rmul -> let (CReal a :<| CReal b :<| ss) = stack s
                         in s { stack = CReal (b*a) <| ss }
                 Rdiv -> let (CReal a :<| CReal b :<| ss) = stack s
                         in s { stack = CReal (b / a) <| ss }
                 Rsh -> let (CI a :<| CReal b :<| ss) = stack s
                        in s { stack = CReal (scale b a) <| ss }
                 Rch -> let (CI n :<| ss) = stack s
                            (cs, ss') = S.splitAt n ss
                        in s { stack = CI (selectCR (map (\(CReal x) -> x) (toList cs))) <| ss' }
                 Entc -> s
                 Lvc _ -> s
                 _ -> s
      put s'
      return True

interpret :: Seq Op -> PgmState -> [PgmState]
interpret p s =
  let (cont, s') = runState (step p) s
  in if cont then s' : interpret p s' else []

s :: PgmState
s = PgmState { pc = 0, stack = Empty, rstack = Empty, pstack = Empty }

main :: IO ()
main = do
  debug prg 0 Empty

debug prg pc stack = mapM_ (putStrLn . show) $ interpret prg (PgmState pc stack Empty Empty)

prg :: Seq Op
prg = fromList [Ipush 1, Dup 1 1, Rot 2 1, Pop 1]

bounded :: Seq Op
bounded = fromList
  [ Entc          -- x p 
  , Ipush 1, Zconv, Rconv, Dup 2 1, Rsh           -- x p 2^p
  , Dup 3 1       -- x p 2^p x
  , Rot 2 1       -- x p x 2^p
  , Dup 2 2       -- x p x 2^p x 2^p
  , Rneg          -- x p x 2^p x -2^p
  , Radd          -- x p x 2^p x-2^p
  , Rot 3 2       -- x p sm x 2^p
  , Rot 2 1       -- x p sm 2^p x
  , Rneg          -- x p sm 2^p -x
  , Radd          -- x p sm 2^p-x
  , Ipush 2
  , Rch         -- x p i
  , Ret
  -- sm = T: x <  2^p => i = 1, -x
  -- sp = T: x > -2^p => i = 2,  x
  , Ipush (-1)      -- x p i -1
  , Iadd          -- x p i-1
  , Ineg          -- x p -(i-1)
  , Zconv         -- x p -(i-1)
  , Rconv         -- X p -(i-1)
  , Dup 3 1       -- x p -(i-1) x
  , Rmul          -- x p y
  , Lvc 1         -- x y
  , Rot 2 1       -- y x
  , Pop 1         -- y
  , Ret
  -- bounded:        -- x p                        [R Z]
  , Rot 2 1       -- p x
  , Scall 0     -- p |x|
  , Dup 2 1       -- p |x| p
  , Ipush 1       -- p |x| p 1
  , Zconv         -- p |x| p 1
  , Rconv         -- p |x| p 1
  , Rot 2 1       -- p |x| 1 p
  , Dup 2 2       -- p |x| 1 p 1 p
  , Ipush (-1)      -- p |x| 1 p 1 p -1
  , Iadd          -- p |x| 1 p 1 p-1
  , Rsh           -- p |x| 1 p 2^(p-1)
  , Dup 4 1       -- p |x| 1 p 2^(p-1) |x|
  , Rot 2 1       -- p |x| 1 p |x| 2^(p-1)
  , Rneg          -- p |x| 1 p |x| -2^(p-1)
  , Radd          -- p |x| 1 p |x|-2^(p-1)
  --, Rsgn          -- p |x| 1 p s_(p-1)
  --, Knot          -- p |x| 1 p lt_(p-1)
  , Rot 4 3       -- p lt_(p-1) |x| 1 p
  , Rsh           -- p lt_(p-1) |x| 2^p
  , Rneg          -- p lt_(p-1) |x| -2^p
  , Radd          -- p lt_(p-1) |x|-2^p
  --, Rsgn          -- p lt_(p-1) s_p             [Z K K]
  --, Knot          -- p lt_(p-1) lt_p
  , Rch         -- p z                        [Z i64]
  -- lt_(p-1) =  1               => z =  0
  --                   lt_p = 1  => z =  1
  -- lt_(p-1) =  0 and lt_p = 0  => reiterate Limit introducing x
  -- lt_(p-1) = -1 and lt_p = 0  => reiterate Limit introducing x
  -- lt_(p-1) =  0 and lt_p = -1 => reiterate Limit introducing x
  -- lt_(p-1) = -1 and lt_p = -1 => z = -1
  , Ipush 1       -- p z 1
  , Iadd          -- p z+1
  , Isgn          -- p sgn(z+1)
  , Rot 2 1       -- z p
  , Pop 1         -- z
  , Ret
  -- main:
  , Ipush 1       -- 1
  , Entc          -- 1 p
  , Dup 2 1       -- 1 p 1
  , Zconv         -- 1 p 1
  , Rconv         -- 1 p 1
  , Ipush 2       -- 1 p 1 2
  , Scall 27 -- 1 p z
  , Lvc 1         -- 1 z
  , Rot 2 1       -- z 1
  , Pop 1         -- z
  ]
  
