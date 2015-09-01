{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Generator where

import           Control.Applicative        ((<$>), (*>))
import           Control.Lens
import           Control.Monad.Trans.Except
import           Control.Monad.State
import qualified Data.Map                   as Map
import           Data.Map                   (Map)


import           CodeGen.ASM2               as ASM
import           CodeGen.Vectors            as V
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
                   , _avAddrs  :: [Addr]
                   , _avLab    :: Int
                   } deriving (Show, Eq, Ord)

makeLenses ''GeneratorData


newtype GeneratorError = GeneratorError { _err :: String } deriving (Show, Eq, Ord)
makeLenses ''GeneratorError


type GeneratorState a  = ExceptT GeneratorError (State GeneratorData) a


--------------------------------------------------------------
-- Processing AST
--------------------------------------------------------------
runASTTranslation :: AST.Module -> GeneratorData
runASTTranslation ast = execState (runExceptT $ processAST ast) (emptyState (fmap Addr [1..100]) 10)


processAST :: AST.Module -> GeneratorState () -- ASM.ASMCode
processAST ast = mapM_ processExpr ast


processExpr :: AST.Expr -> GeneratorState ()
processExpr expr = case expr of
    AST.Lit i           -> push  (fromIntegral i      :: Int  ) -- TODO change integers to ints
    AST.VecLit is       -> pushV (map fromIntegral is :: [Int])
    AST.VarE var        -> lookupVar var
    AST.Decl nm tp expr -> createVar nm tp expr
    AST.Assign var expr -> processExpr expr *> storeInstr var
    AST.BinOp op l r    -> processExpr l *> processExpr r *> processOp op
    AST.If cond ts fs   -> processIf cond ts fs
    _                   -> throwE $ GeneratorError "Instruction not yet implemented"


processIf :: AST.Expr -> [AST.Expr] -> [AST.Expr] -> GeneratorState ()
processIf cond ts fs = do
    labEnd  <- ASM.Lab <$> getNextLabel
    labElse <- ASM.Lab <$> getNextLabel

    processExpr cond
    pushASMInstr $ ASM.JumpZ labElse
    mapM_ processExpr ts
    pushASMInstr $ ASM.Jump  labEnd
    pushASMInstr $ ASM.Label labElse
    mapM_ processExpr fs
    pushASMInstr $ ASM.Label labEnd


processOp :: AST.Op -> GeneratorState ()
processOp op = pushASMInstr $ case op of
                   AST.Add _ -> ASM.Add
                   AST.Sub _ -> ASM.Sub
                   AST.Mul _ -> ASM.Mul
                   AST.Div _ -> ASM.Div


createVar :: AST.VarName -> AST.Type -> AST.Expr -> GeneratorState ()
createVar nm tp expr = do
    let size = case tp of
            AST.Scalar -> 1
            AST.Vector a -> fromInteger . toInteger $ a :: Int
    addr <- getAvailableAddr size
    processExpr expr
    pushVarInfo  $ VarInfo nm addr -- isVec
    pushASMInstr $ ASM.Store addr


--------------------------------------------------------------
-- Utility state functions
--------------------------------------------------------------

pushASMInstr :: ASM.ASMInstruction -> GeneratorState ()
pushASMInstr instr = do
    genData <- get
    let newData = genData & asmCode %~ (++ [instr])
    put newData


getAvailableAddr :: Int -> GeneratorState Addr
getAvailableAddr size = do
    st <- get
    let addr = st ^. avAddrs
    if length addr < size
      then
        throwE $ GeneratorError "No more address space for new variables"
      else
          do
            let newSt = st & avAddrs .~ (drop size addr )
            put newSt
            return . head $ addr


getNextLabel :: GeneratorState Int
getNextLabel = do
    st <- get
    let lab   = st ^. avLab
        newSt = st & avLab %~ (+1)
    put newSt
    return lab


pushVarInfo :: VarInfo -> GeneratorState ()
pushVarInfo vInfo = do
    st <- get
    let newSt = st & symTable %~ (Map.insert (vInfo ^. varName) vInfo)
    put newSt


storeInstr :: AST.VarName -> GeneratorState ()
storeInstr nm = do
    adr <- lookupAddr nm
    pushASMInstr $ ASM.Store adr

lookupAddr :: AST.VarName -> GeneratorState Addr
lookupAddr nm = do
    st <- get
    let mMap = st ^. symTable
    case Map.lookup nm mMap of
      Nothing -> error "Assigning to non-existing variable"
      Just (VarInfo _ n ) -> return n

lookupVar :: AST.VarName -> GeneratorState ()
lookupVar var = do
    st <- get
    case Map.lookup var (st ^. symTable) of
        Nothing    -> throwE . GeneratorError $ "Using undeclared variable name: " ++ var
        Just vInfo -> pushASMInstr $ ASM.Load (vInfo ^. varAddr) -- TODO this function can be
                                                                  -- generic with respect to var type

push :: Int -> GeneratorState ()
push i = pushASMInstr . Push . scalar $ i


pushV :: [Int] -> GeneratorState ()
pushV is = mapM_ (pushASMInstr . Push) chunks
    where chunks = pack is


withLoop :: Int -> [AST.Expr] -> GeneratorState ()
withLoop reps exprs = do
    labStart <- ASM.Lab <$> getNextLabel
    labEnd   <- ASM.Lab <$> getNextLabel

    let (ctrDecl, decr, cond) = AST.loopIf (labEnd ^. ASM.labNum) reps

    processExpr ctrDecl
    pushASMInstr $ ASM.Label labStart
    processExpr cond
    pushASMInstr $ ASM.JumpZ labEnd

    mapM_ processExpr exprs

    processExpr decr
    pushASMInstr $ ASM.Jump  labStart


emptyState = GeneratorData [] [] Map.empty
