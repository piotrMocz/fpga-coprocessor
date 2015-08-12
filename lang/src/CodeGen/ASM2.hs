{-# LANGUAGE FlexibleInstances    #-}

module CodeGen.ASM2 where

import Control.Lens
import Control.Monad (sequence)
import Data.Functor  ((<$>))
import Data.List     (intercalate)


data Addr = SAddr Int | VAddr Int deriving (Show, Eq, Ord)

newtype SVal  = SVal  Int    deriving (Eq, Ord)
newtype VVal  = VVal  [SVal] deriving (Eq, Ord)


sMinAddr = 0
sMaxAddr = 32
vMinAddr = 0
vMaxAddr = 32


sMinVal = -128
sMaxVal = 127


createSAddr :: Int -> Maybe Addr
createSAddr n = if n >= sMinAddr && n < sMaxAddr then Just $ SAddr n
                                                 else Nothing


createVAddr :: Int -> Maybe Addr
createVAddr n = if n >= vMinAddr && n < vMaxAddr then Just $ VAddr n
                                                 else Nothing


createSVal :: Int -> Maybe SVal
createSVal v = if v >= sMinVal && v < sMaxVal then Just $ SVal v
                                              else Nothing


createVVal :: [Int] -> Maybe VVal
createVVal vs = VVal <$> (sequence $ map createSVal vs)


data ASMInstruction = LoadS  { addr    :: Addr  }
                    | LoadV  { vaddr   :: Addr  }
                    | StoreS { saddr   :: Addr  }
                    | StoreV { vaddr   :: Addr  }
                    | PushS  { sval    :: SVal  }
                    | PushV  { vval    :: VVal  }
                    | AddS
                    | AddV
                    | SubS
                    | SubV
                    | MulS
                    | MulV
                    | DivS
                    | DivV deriving (Eq, Ord)


type ASMCode = [ASMInstruction]


class MakeASM a where
    makeASM :: a -> String


instance MakeASM Addr where
    makeASM (SAddr a) = show a
    makeASM (VAddr a) = show a

instance MakeASM SVal where
    makeASM (SVal a)     = show a

instance MakeASM VVal where
    makeASM (VVal svals) = "[" ++ (map makeASM svals & intercalate ",") ++ "]"


instance MakeASM ASMInstruction where
    makeASM (LoadS  addr) = "LDS "   ++ makeASM addr
    makeASM (LoadV  addr) = "LDV "   ++ makeASM addr
    makeASM (StoreS addr) = "STS "   ++ makeASM addr
    makeASM (StoreV addr) = "STV "   ++ makeASM addr
    makeASM (PushS  val ) = "PUSHS " ++ makeASM val
    makeASM (PushV  val ) = "PUSHV " ++ makeASM val
    makeASM  AddS         = "ADDS"
    makeASM  AddV         = "ADDV"
    makeASM  SubS         = "SUBS"
    makeASM  SubV         = "SUBV"
    makeASM  MulS         = "MULS"
    makeASM  MulV         = "MULV"
    makeASM  DivS         = "DIVS"
    makeASM  DivV         = "DIVV"


instance Show ASMInstruction where
    show instr = makeASM instr

instance MakeASM [ASMInstruction] where
    makeASM instrs = unlines $ fmap show instrs