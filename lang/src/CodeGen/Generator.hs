{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Generator where

import           Control.Lens
import           Control.Monad.Trans.Except
import           Control.Monad.State
import           Control.Monad              (sequence, mapM)
import qualified Data.Map                   as Map
import           Data.Map                   (Map)
import           Data.Monoid

import           CodeGen.ASM2               as ASM
import qualified Parser.AST                 as AST



type SymTable = Map String VarInfo


data VarType = Scalar | Vector deriving (Show, Eq, Ord)


data VarInfo = VarInfo {
               _varName :: String
             , _varType :: VarType
             , _varAddr :: Addr
             } deriving (Show, Eq, Ord)

makeLenses ''VarInfo


data GeneratorData = GeneratorData {
                     _asmCode  :: ASM.ASMCode
                   , _modAST   :: AST.Module
                   , _symTable :: SymTable
                   , _avAddrsS :: [Addr]
                   , _avAddrsV :: [Addr]
                   } deriving (Show, Eq, Ord)

makeLenses ''GeneratorData


newtype GeneratorError = GeneratorError { _err :: String } deriving (Show, Eq, Ord)
makeLenses ''GeneratorError


type GeneratorState a  = ExceptT GeneratorError (State GeneratorData) a


--------------------------------------------------------------
-- Processing AST
--------------------------------------------------------------

processAST :: AST.Module -> GeneratorState () -- ASM.ASMCode
processAST ast = mapM_ processExpr ast


processExpr :: AST.Expr -> GeneratorState ()
processExpr expr = case expr of
    AST.Lit i           -> pushS (fromIntegral i      :: Int  ) -- TODO change integers to ints
    AST.VecLit is       -> pushV (map fromIntegral is :: [Int])
    AST.VarE var        -> lookupVar var
    AST.Assign var expr -> createVar var expr
    _                   -> throwE $ GeneratorError "Instruction not yet implemented"


createVar :: AST.Var -> AST.Expr -> GeneratorState ()
createVar var expr = do
    addr <- getAvailableAddrS
    processExpr expr
    pushVarInfo $ VarInfo var Scalar addr
    pushASMInstr $ ASM.StoreS addr


--------------------------------------------------------------
-- Utility state functions
--------------------------------------------------------------

pushASMInstr :: ASM.ASMInstruction -> GeneratorState ()
pushASMInstr instr = do
    genData <- get
    let newData = genData & asmCode %~ (instr :)
    put newData


getAvailableAddrS :: GeneratorState Addr
getAvailableAddrS = do
    st <- get
    let addr = st ^. avAddrsS
    case addr of
        []     -> throwE $ GeneratorError "No more address space for new variables"
        (a:as) -> do
            let newSt = st & avAddrsS .~ as
            put newSt
            return a


getAvailableAddrV :: GeneratorState Addr
getAvailableAddrV = do
    st <- get
    let addr = st ^. avAddrsV
    case addr of
        []     -> throwE $ GeneratorError "No more address space for new variables"
        (a:as) -> do
            let newSt = st & avAddrsV .~ as
            put newSt
            return a


pushVarInfo :: VarInfo -> GeneratorState ()
pushVarInfo vInfo = do
    st <- get
    let newSt = st & symTable %~ (Map.insert (vInfo ^. varName) vInfo)
    put st


lookupVar :: AST.Var -> GeneratorState ()
lookupVar var = do
    st <- get
    case Map.lookup var (st ^. symTable) of
        Nothing    -> throwE . GeneratorError $ "Using undeclared variable name: " ++ var
        Just vInfo -> pushASMInstr $ ASM.LoadS (vInfo ^. varAddr) -- TODO this function can be
                                                                  -- generic with respect to var type


pushS :: Int -> GeneratorState ()
pushS v = case ASM.createSVal v of
    Nothing  -> throwE $ GeneratorError "Scalar value out of range"
    Just val -> pushASMInstr $ PushS val


pushV :: [Int] -> GeneratorState ()
pushV vs = case ASM.createVVal vs of
    Nothing   -> throwE $ GeneratorError "Vector value out of range"
    Just vals -> pushASMInstr $ PushV vals


emptyState = GeneratorData [] [] Map.empty