module CodeGen.ASM (
  createSAddr,
  createVAddr,
  createSReg,
  createVReg,
  createSVal,
  createVVal,
  ASMInstruction(..),
  ASMCode
) where

import Control.Monad (sequence)
import Data.Functor  ((<$>))

newtype SAddr = SAddr Int    deriving (Eq, Ord)
newtype VAddr = VAddr Int    deriving (Eq, Ord)
newtype SReg  = SReg  Int    deriving (Eq, Ord)
newtype VReg  = VReg  Int    deriving (Eq, Ord)
newtype SVal  = SVal  Int    deriving (Eq, Ord)
newtype VVal  = VVal  [SVal] deriving (Eq, Ord)

sMinAddr = 0
sMaxAddr = 32
vMinAddr = 0
vMaxAddr = 32

sMinReg = 0
sMaxReg = 32
vMinReg = 0
vMaxReg = 32

sMinVal = -128
sMaxVal = 127


createSAddr :: Int -> Maybe SAddr
createSAddr n = if n >= sMinAddr && n < sMaxAddr then Just $ SAddr n
                                                 else Nothing

createVAddr :: Int -> Maybe VAddr
createVAddr n = if n >= vMinAddr && n < vMaxAddr then Just $ VAddr n
                                                 else Nothing

createSReg :: Int -> Maybe SReg
createSReg n = if n >= sMinReg && n < sMaxReg then Just $ SReg n
                                              else Nothing

createVReg :: Int -> Maybe VReg
createVReg n = if n >= vMinReg && n < vMaxReg then Just $ VReg n
                                              else Nothing

createSVal :: Int -> Maybe SVal
createSVal v = if v >= sMinVal && v < sMaxVal then Just $ SVal v
                                              else Nothing

createVVal :: [Int] -> Maybe VVal
createVVal vs = VVal <$> (sequence $ map createSVal vs)


data ASMInstruction = MovS   { sregSrc :: SReg,  sregDst :: SReg                  }
                    | MovV   { vregSrc :: SReg,  sregDst :: SReg                  }
                    | LoadS  { addr    :: SAddr, sreg    :: SReg                  }
                    | LoadV  { vaddr   :: VAddr, vreg    :: VReg                  }
                    | StoreS { sreg    :: SReg,  saddr   :: SAddr                 }
                    | StoreV { vreg    :: VReg,  vaddr   :: VAddr                 }
                    | PutS   { sval    :: SVal,  sreg    :: SReg                  }
                    | PutV   { vval    :: VVal,  vreg    :: VReg                  }
                    | AddS   { sreg1   :: SReg,  sreg2   :: SReg, sregDst :: SReg }
                    | AddV   { vreg1   :: VReg,  vreg2   :: VReg, vregDst :: VReg }
                    | SubS   { sreg1   :: SReg,  sreg2   :: SReg, sregDst :: SReg }
                    | SubV   { vreg1   :: VReg,  vreg2   :: VReg, vregDst :: VReg }
                    | MulS   { sreg1   :: SReg,  sreg2   :: SReg, sregDst :: SReg }
                    | MulV   { vreg1   :: VReg,  vreg2   :: VReg, vregDst :: VReg }
                    | DivS   { sreg1   :: SReg,  sreg2   :: SReg, sregDst :: SReg }
                    | DivV   { vreg1   :: VReg,  vreg2   :: VReg, vregDst :: VReg } deriving (Eq, Ord)

type ASMCode = [ASMInstruction]


instance Show SAddr where
  show (SAddr a) = show a
instance Show VAddr where
  show (VAddr a) = show a
instance Show SReg where
  show (SReg a)  = show a
instance Show VReg where
  show (VReg a)  = show a
instance Show SVal where
  show (SVal a)  = show a
instance Show VVal where
  show (VVal a)  = show a


instance Show ASMInstruction where
  show (LoadS  addr reg)          = "LD "   ++ (show addr) ++ " " ++ (show reg)
  show (LoadV  addr reg)          = "LDV "  ++ (show addr) ++ " " ++ (show reg)
  show (StoreS reg  addr)         = "ST "   ++ (show reg)  ++ " " ++ (show addr)
  show (StoreV addr reg)          = "STV "  ++ (show reg)  ++ " " ++ (show addr)
  show (PutS   val  reg)          = "PUTS " ++ (show val)  ++ " " ++ (show reg)
  show (PutV   val  reg)          = "PUTV " ++ (show val)  ++ " " ++ (show reg)
  show (AddS   reg1 reg2 regDest) = "ADD "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (AddV   reg1 reg2 regDest) = "ADDV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (SubS   reg1 reg2 regDest) = "SUB "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (SubV   reg1 reg2 regDest) = "SUBV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (MulS   reg1 reg2 regDest) = "MUL "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (MulV   reg1 reg2 regDest) = "MULV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (DivS   reg1 reg2 regDest) = "DIV "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (DivV   reg1 reg2 regDest) = "DIVV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  
  