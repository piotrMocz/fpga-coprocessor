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



data VarInfo = VarInfo {
               _varName :: String
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
runASTTranslation :: AST.Module -> GeneratorData
runASTTranslation ast = execState (runExceptT $ processAST ast) (emptyState [] [])


processAST :: AST.Module -> GeneratorState () -- ASM.ASMCode
processAST ast = mapM_ processExpr ast


processExpr :: AST.Expr -> GeneratorState ()
processExpr expr = case expr of
    AST.Lit i           -> pushV [fromIntegral i      :: Int  ] -- TODO change integers to ints
    AST.VecLit is       -> pushV (map fromIntegral is :: [Int])
    AST.VarE var        -> lookupVar var
    AST.Assign var expr -> createVar var expr
    AST.BinOp op l r    -> do {processExpr l; processExpr r; processOp op;}
    _                   -> throwE $ GeneratorError "Instruction not yet implemented"

processOp :: AST.Op -> GeneratorState ()
processOp op = pushASMInstr $ case op of
                   AST.Add -> ASM.Add
                   AST.Sub -> ASM.Sub
                   AST.Mul -> ASM.Mul
                   AST.Div -> ASM.Div


createVar :: AST.Var -> AST.Expr -> GeneratorState ()
createVar var expr = do
    addr <- getAvailableAddrS
    processExpr expr
    pushVarInfo $ VarInfo var addr
    pushASMInstr $ ASM.Store addr


--------------------------------------------------------------
-- Utility state functions
--------------------------------------------------------------

pushASMInstr :: ASM.ASMInstruction -> GeneratorState ()
pushASMInstr instr = do
    genData <- get
    let newData = genData & asmCode %~ (++ [instr])
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
        Just vInfo -> pushASMInstr $ ASM.Load (vInfo ^. varAddr) -- TODO this function can be
                                                                  -- generic with respect to var type

pushV :: [Int] -> GeneratorState ()
pushV vs = case ASM.createVVal vs of
    Nothing   -> throwE $ GeneratorError "Vector value out of range"
    Just vals -> pushASMInstr $ Push vals


emptyState = GeneratorData [] [] Map.empty