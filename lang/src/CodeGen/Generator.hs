{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Generator where

import Control.Monad.State
import Control.Lens
import Data.Monoid

import CodeGen.ASM as ASM
import Parser.AST  as AST


data GeneratorData = GeneratorData {
                     _asmCode    :: ASM.ASMCode
                   , _moduleAST  :: AST.Module
                   } deriving (Show, Eq, Ord)

makeLenses ''GeneratorData

type GeneratorState a = State GeneratorData a


pushASMInstr :: ASM.ASMInstruction -> GeneratorState ()
pushASMInstr instr = do
  genData <- get
  let newData = genData & asmCode %~ (instr :)
  put newData


popExpr :: GeneratorState (Maybe AST.Expr)
popExpr = do
  genData <- get
  if null $ genData ^. moduleAST
    then return Nothing
    else do
      let (e:es)  = genData ^. moduleAST
          newData = genData &  moduleAST .~ es
      put newData
      return $ Just e    

