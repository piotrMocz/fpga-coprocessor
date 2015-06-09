module CodeGen.ASM where

newtype SAddr = SAddr Int deriving (Eq, Ord)
newtype VAddr = VAddr Int deriving (Eq, Ord)
newtype SReg  = SReg  Int deriving (Eq, Ord)
newtype VReg  = VReg  Int deriving (Eq, Ord)

sMinAddr = 0
sMaxAddr = 32
vMinAddr = 0
vMaxAddr = 32

sMinReg = 0
sMaxReg = 32
vMinReg = 0
vMaxReg = 32


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


data ASMInstruction = LoadInstr   { addr  :: SAddr, reg   :: SReg                   }
                    | VLoadInstr  { vaddr :: VAddr, vreg  :: VReg                   }
                    | StoreInstr  { reg   :: SReg,  addr  :: SAddr                  }
                    | VStoreInstr { vreg  :: VReg,  vaddr :: VAddr                  }
                    | AddInstr    { reg1  :: SReg,  reg2  :: SReg, regDest  :: SReg }
                    | VAddInstr   { vreg1 :: VReg,  vreg2 :: VReg, vregDest :: VReg }
                    | SubInstr    { reg1  :: SReg,  reg2  :: SReg, regDest  :: SReg }
                    | VSubInstr   { vreg1 :: VReg,  vreg2 :: VReg, vregDest :: VReg }
                    | MulInstr    { reg1  :: SReg,  reg2  :: SReg, regDest  :: SReg }
                    | VMulInstr   { vreg1 :: VReg,  vreg2 :: VReg, vregDest :: VReg }
                    | DivInstr    { reg1  :: SReg,  reg2  :: SReg, regDest  :: SReg }
                    | VDivInstr   { vreg1 :: VReg,  vreg2 :: VReg, vregDest :: VReg } deriving (Eq, Ord)


instance Show SAddr where
  show (SAddr a) = show a
instance Show VAddr where
  show (VAddr a) = show a
instance Show SReg where
  show (SReg a)  = show a
instance Show VReg where
  show (VReg a)  = show a


instance Show ASMInstruction where
  show (LoadInstr   addr reg)  = "LD "  ++ (show addr) ++ " " ++ (show reg)
  show (VLoadInstr  addr reg)  = "LDV " ++ (show addr) ++ " " ++ (show reg)
  show (StoreInstr  reg  addr) = "ST "  ++ (show reg)  ++ " " ++ (show addr)
  show (VStoreInstr addr reg)  = "STV " ++ (show reg)  ++ " " ++ (show addr)
  show (AddInstr    reg1 reg2 regDest) = "ADD "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (VAddInstr   reg1 reg2 regDest) = "ADDV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (SubInstr    reg1 reg2 regDest) = "SUB "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (VSubInstr   reg1 reg2 regDest) = "SUBV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (MulInstr    reg1 reg2 regDest) = "MUL "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (VMulInstr   reg1 reg2 regDest) = "MULV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (DivInstr    reg1 reg2 regDest) = "DIV "  ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  show (VDivInstr   reg1 reg2 regDest) = "DIVV " ++ (show reg1) ++ " " ++ (show reg2) ++ " " ++ (show regDest)
  
  
