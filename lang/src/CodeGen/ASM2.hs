{-# LANGUAGE FlexibleInstances    #-}

module CodeGen.ASM2 where

import Control.Lens
import Control.Monad (sequence)
import Data.Functor  ((<$>))
import Data.List     (intercalate)


data Addr = VAddr Int deriving (Show, Eq, Ord)

newtype SVal  = SVal  Int    deriving (Eq, Ord)
newtype VVal  = VVal  [SVal] deriving (Eq, Ord)


sMinAddr = 0
sMaxAddr = 32
vMinAddr = 0
vMaxAddr = 32


sMinVal = -128
sMaxVal = 127



createVAddr :: Int -> Maybe Addr
createVAddr n = if n >= vMinAddr && n < vMaxAddr then Just $ VAddr n
                                                 else Nothing

createSVal :: Int -> Maybe SVal
createSVal v = if v >= sMinVal && v < sMaxVal then Just $ SVal v
                                              else Nothing

createVVal :: [Int] -> Maybe VVal
createVVal vs = VVal <$> (sequence $ map createSVal vs)


data ASMInstruction = Load  { addr    :: Addr  }
                    | Store { vaddr   :: Addr  }
                    | Push  { vval    :: VVal  }
                    | Add
                    | Sub
                    | Mul
                    | Div deriving (Eq, Ord)


type ASMCode = [ASMInstruction]


class MakeASM a where
    makeASM :: a -> String


instance MakeASM Addr where
    makeASM (VAddr a) = show a

instance MakeASM SVal where
    makeASM (SVal a)     = show a

instance MakeASM VVal where
    makeASM (VVal svals) = "[" ++ (map makeASM svals & intercalate ",") ++ "]"


instance MakeASM ASMInstruction where
    makeASM (Load  addr) = "LD "   ++ makeASM addr
    makeASM (Store addr) = "ST "   ++ makeASM addr
    makeASM (Push  val ) = "PUSH " ++ makeASM val
    makeASM  Add         = "ADD"
    makeASM  Sub         = "SUB"
    makeASM  Mul         = "MUL"
    makeASM  Div         = "DIV"


instance Show ASMInstruction where
    show instr = makeASM instr

instance MakeASM [ASMInstruction] where
    makeASM instrs = unlines $ fmap show instrs