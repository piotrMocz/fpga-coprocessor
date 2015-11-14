{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TemplateHaskell           #-}


module CodeGen.ASM where

import Control.Lens
import Control.Monad   (sequence)
import Data.Functor    ((<$>))
import Data.List       (intercalate)

import CodeGen.Vectors
import Data.Int        (Int16)

type ConstStorage = [ConstData]

data ConstData = ConstScalar Int
               | ConstVector (Int, Int, Int, Int, Int, Int, Int, Int) deriving (Show, Ord, Eq)

newtype Addr  = Addr  Int              deriving (Show, Eq, Ord)
newtype SVal  = SVal  Int              deriving (Show, Eq, Ord)
newtype VVal  = VVal  [SVal]           deriving (Show, Eq, Ord)
newtype Lab   = Lab { _labNum :: Int } deriving (Show, Eq, Ord)
makeLenses ''Lab

vMinAddr = 0
vMaxAddr = 32

sMinVal = -128
sMaxVal = 127


createAddr :: Int -> Maybe Addr
createAddr n = if n >= vMinAddr && n < vMaxAddr then Just $ Addr n
                                                 else Nothing

createSVal :: Int -> Maybe SVal
createSVal v = if v >= sMinVal && v < sMaxVal then Just $ SVal v
                                              else Nothing

createVVal :: [Int] -> Maybe VVal
createVVal vs = VVal <$> (sequence $ map createSVal vs)


mkLabel :: Int -> ASMInstruction
mkLabel = Label . Lab

unLabel :: ASMInstruction -> Int
unLabel (Label (Lab i)) = i
unLabel _               = error "Cannot unlabel a nonlabel"


data ASMInstruction = Load    { addr  :: Addr  , size :: Int }   -- 0
                    | Store   { vaddr :: Addr  , size :: Int }   -- 1
                    | Push    { vaddr :: Addr                }   -- 2
                    | MovS1                        -- pop from the main stack and push onto the 1st helper stack
                    | MovS2                        -- as above but the 2nd stack
                    | JumpZ   { tgt   :: Lab   }   -- --
                    | Jump    { tgt   :: Lab   }   -- --
                    | Label   { lab   :: Lab   }   -- 3
                    | JumpIPZ { ipVal :: Int   }   -- 4
                    | JumpIP  { ipVal :: Int   }   -- 5
                    | Add  -- ordinary scalar addition
                    | Sub
                    | Mul
                    | Div
                    | AddS -- add from two helper stacks and push the result onto the main stack
                    | SubS -- as above, subtract
                    | MulS -- multiply
                    | DivS -- divide
                    deriving (Eq, Ord)


type ASMCode = [ASMInstruction]


class MakeASM a where
    makeASM :: a -> String


instance MakeASM Int where
    makeASM = show

instance MakeASM Chunk where
    makeASM = show

instance MakeASM Addr where
    makeASM (Addr a) = show a

instance MakeASM SVal where
    makeASM (SVal a)     = show a

instance MakeASM VVal where
    makeASM (VVal svals) = "[" ++ (map makeASM svals & intercalate ",") ++ "]"

instance MakeASM Lab where
    makeASM (Lab i) = show i


instance MakeASM ASMInstruction where
    makeASM (Load    addr size) = "LD "    ++ makeASM addr ++ " size " ++ (show size)
    makeASM (Store   addr size) = "ST "    ++ makeASM addr ++ " size " ++ (show size)
    makeASM (Push    val      ) = "PUSH "  ++ makeASM val
    makeASM  MovS1              = "MOVS1"
    makeASM  MovS2              = "MOVS2"
    makeASM (JumpZ   tgt      ) = "JUMPZ " ++ makeASM tgt
    makeASM (Jump    tgt      ) = "JUMP "  ++ makeASM tgt
    makeASM (Label   lab      ) = "LAB "   ++ makeASM lab
    makeASM (JumpIPZ tgt      ) = "JUMPZ " ++ makeASM tgt
    makeASM (JumpIP  tgt      ) = "JUMP "  ++ makeASM tgt
    makeASM  Add                = "ADD"
    makeASM  Sub                = "SUB"
    makeASM  Mul                = "MUL"
    makeASM  Div                = "DIV"
    makeASM  AddS               = "ADDS"
    makeASM  SubS               = "SUBS"
    makeASM  MulS               = "MULS"
    makeASM  DivS               = "DIVS"


instance Show ASMInstruction where
    show instr = makeASM instr


instance MakeASM a => MakeASM [a] where
    makeASM instrs = unlines $ map makeASM instrs
