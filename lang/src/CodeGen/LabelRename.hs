{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CodeGen.LabelRename where

import Control.Lens

import qualified CodeGen.ASM       as ASM
import           CodeGen.ASM       (MakeASM, makeASM, Lab(Lab), ASMInstruction(..), ASMCode)
import           CodeGen.Generator (GeneratorData)
import qualified CodeGen.Generator as Gen


type NumberedInstr = (Int, ASMInstruction)
type NumberedCode  = [NumberedInstr]


renameLabels :: ASMCode -> NumberedCode
renameLabels asmCode = renameLabels' labelInstrs instrsNum
    where instrsNum   = zip [0..] asmCode
          labelInstrs = filter isLabelInstr instrsNum
          isLabelInstr (_, Label _) = True
          isLabelInstr _                = False


renameLabels' :: NumberedCode -> NumberedCode -> NumberedCode
renameLabels' []     instrs = instrs
renameLabels' (l:ls) instrs = renameLabels' ls (processLabel l instrs)


processLabel :: NumberedInstr -> NumberedCode -> NumberedCode
processLabel (labTgt, Label (Lab labNum)) instrs = map lab2IP instrs
    where lab2IP i@(n, (JumpZ (Lab l))) = if l == labNum then (n, JumpIPZ labTgt) else i
          lab2IP i@(n, (Jump  (Lab l))) = if l == labNum then (n, JumpIP  labTgt) else i
          lab2IP i                          = i


instance MakeASM NumberedInstr where
    makeASM (i, a) =  "#" ++ show i ++ ":  " ++ makeASM a
