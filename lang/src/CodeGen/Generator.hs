{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Generator where

import           Control.Applicative        ((<$>), (*>))
import           Control.Lens
import           Control.Monad              (replicateM_)
import           Control.Monad.Trans.Except
import           Control.Monad.State
import qualified Data.Map                   as Map
import           Data.Map                   (Map)


import           CodeGen.ASM                as ASM
import           CodeGen.Vectors            as V
import qualified Parser.AST                 as AST



type SymTable = Map String VarInfo


data VarInfo = VarInfo {
               _varName :: String
             , _varAddr :: Addr
             , _varLen  :: Int
             } deriving (Show, Eq, Ord)

makeLenses ''VarInfo


data GeneratorData = GeneratorData {
                     _asmCode   :: ASM.ASMCode
                   , _constData :: ASM.ConstStorage
                   , _modAST    :: AST.Module
                   , _symTable  :: SymTable
                   , _avAddrs   :: [Addr]
                   , _avLab     :: Int
                   } deriving (Show, Eq, Ord)

makeLenses ''GeneratorData


newtype GeneratorError = GeneratorError { _err :: String } deriving (Show, Eq, Ord)
makeLenses ''GeneratorError


type GeneratorState a  = ExceptT GeneratorError (State GeneratorData) a


--------------------------------------------------------------
-- Processing AST
--------------------------------------------------------------
runASTTranslation :: AST.Module -> GeneratorData
runASTTranslation ast = execState (runExceptT $ processAST ast) (emptyState (fmap Addr [0..128]) 10)


processAST :: AST.Module -> GeneratorState () -- ASM.ASMCode
processAST ast = mapM_ processExpr ast


processExpr :: AST.Expr -> GeneratorState ()
processExpr expr = case expr of
    AST.Lit i            -> pushV  (map fromIntegral [0,0,0,0,0,0,0,i] :: [Int]) -- TODO change integers to ints
    AST.VecLit is        -> pushV (map fromIntegral is :: [Int])
    AST.VarE var         -> lookupVar var
    AST.Decl nm tp expr  -> createVar nm tp expr
    AST.Assign var expr  -> processExpr expr *> storeInstr var
    AST.BinOp op l r     -> processExpr l *> processExpr r *> (if AST.isScalarOp op then processOp else unvectorizeOp) op
    AST.If   cond ts fs  -> processIf cond ts fs
    AST.Loop cond body   -> processLoop cond body
    _                    -> throwE $ GeneratorError "Instruction not yet implemented"


processLoop :: AST.Expr -> [AST.Expr] -> GeneratorState ()
processLoop cond body = withLoop cond body


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
                   AST.Add   _ -> ASM.Add
                   AST.Sub   _ -> ASM.Sub
                   AST.Mul   _ -> ASM.Mul
                   AST.Div   _ -> ASM.Div


createVar :: AST.VarName -> AST.Type -> AST.Expr -> GeneratorState ()
createVar nm tp expr = do
    let size = typeSize tp
    addr <- getAvailableAddr size
    let vecLen = (size - 1) `div` 8
    processExpr expr
    pushVarInfo  $ VarInfo nm addr size
    mapM_ (loadInst addr) (reverse [0..vecLen])
      where loadInst (Addr i) offset = pushASMInstr $ ASM.Store (Addr $ i + offset)


{-
storeInstr :: AST.VarName -> GeneratorState ()
storeInstr var = do
     firstAdr <- lookupAddr var
     len <- lookupLen var
     let vecLen = (len - 1) `div` 8
     mapM_ (loadInst firstAdr) [0..vecLen]
      where loadInst (Addr i) offset = pushASMInstr $ ASM.Store (Addr $ i + offset)
      -}
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
    let vecSize = (size-1) `div` 8 + 1
    if length addr < vecSize
      then
        throwE $ GeneratorError "No more address space for new variables"
      else
          do
            let newSt = st & avAddrs .~ (drop vecSize addr )
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
storeInstr var = do
     firstAdr <- lookupAddr var
     len <- lookupLen var
     let vecLen = (len - 1) `div` 8
     mapM_ (loadInst firstAdr) (reverse [0..vecLen])
      where loadInst (Addr i) offset = pushASMInstr $ ASM.Store (Addr $ i + offset)


lookupAddr :: AST.VarName -> GeneratorState Addr
lookupAddr nm = do
    st <- get
    let mMap = st ^. symTable
    case Map.lookup nm mMap of
      Nothing -> error "Assigning to non-existing variable"
      Just (VarInfo _ n _) -> return n


lookupLen :: AST.VarName -> GeneratorState Int
lookupLen nm = do
    st <- get
    let mMap = st ^. symTable
    case Map.lookup nm mMap of
      Nothing -> error "Assigning to non-existing variable"
      Just (VarInfo _ _ l) -> return l

lookupVar :: AST.VarName -> GeneratorState ()
lookupVar var = do
    firstAdr <- lookupAddr var
    len <- lookupLen var
    let vecLen = (len - 1) `div` 8
    mapM_ (loadInst firstAdr) [0..vecLen]
     where loadInst (Addr i) offset = pushASMInstr $ ASM.Load (Addr $ i + offset)

push :: Int -> GeneratorState ()
push i = do
  ptr <- nextAdress
  addScalarToStorage i
  pushASMInstr . Push $ ptr


pushV :: [Int] -> GeneratorState ()
pushV is = mapM_ prepareChunk chunks
    where chunks = pack is

prepareChunk :: Chunk -> GeneratorState ()
prepareChunk ck = do
  ptr <- nextAdress
  addVectorToStorage ck
  pushASMInstr . Push $ ptr

addScalarToStorage :: Int -> GeneratorState ()
addScalarToStorage i = do
    gD <- get
    let newData = gD & constData %~ (++ [ASM.ConstScalar i])
    put newData


addVectorToStorage :: Chunk -> GeneratorState ()
addVectorToStorage ck = do
    gD <- get
    let newData = gD & constData %~ (++ [ASM.ConstVector (ck ^. body)])
    put newData


nextAdress :: GeneratorState ASM.Addr
nextAdress = do
  st <- get
  let nVec    = length $ filter (not . isScalar) (st ^. constData)
      nScalar = length $ filter isScalar (st ^. constData)
  return . Addr $ nVec + nScalar

isScalar :: ASM.ConstData -> Bool
isScalar (ASM.ConstScalar _) = True
isScalar _ = False

withLoopInt :: Int -> [AST.Expr] -> GeneratorState ()
withLoopInt reps = withLoop (AST.Lit $ fromIntegral reps)


withLoop :: AST.Expr -> [AST.Expr] -> GeneratorState ()
withLoop cond exprs = do
    labStart <- ASM.Lab <$> getNextLabel
    labEnd   <- ASM.Lab <$> getNextLabel

    let (lctrDecl, ldecr, lcond) = AST.loopIf (labEnd ^. ASM.labNum) cond

    processExpr lctrDecl
    pushASMInstr $ ASM.Label labStart
    processExpr lcond
    pushASMInstr $ ASM.JumpZ labEnd

    mapM_ processExpr exprs

    processExpr ldecr
    pushASMInstr $ ASM.Jump  labStart
    pushASMInstr $ ASM.Label labEnd


emptyState = GeneratorData [] [] [] Map.empty


-- translate a vectorized binop into a sequence of instructions, operating on 3 stacks
-- TODO might put a loop here, but this is arguably simpler to process. short vectors, nice vectors.
unvectorizeOp :: AST.Op -> GeneratorState ()
unvectorizeOp op = do
    let len = V.chunkCnt $ AST.opLen op
    len `times` (pushASMInstr ASM.MovS1)  -- push the first vector
    len `times` (pushASMInstr ASM.MovS2)  -- push the second vector

    len `times` (pushASMInstr $ getTwoStackOp op)
    case op of
      AST.DotPr _ -> (len - 1) `times` (pushASMInstr ASM.Add)
      _ -> return ()



getTwoStackOp :: AST.Op -> ASM.ASMInstruction
getTwoStackOp (AST.Add   _)   = ASM.AddS
getTwoStackOp (AST.Sub   _)   = ASM.SubS
getTwoStackOp (AST.Mul   _)   = ASM.MulS
getTwoStackOp (AST.Div   _)   = ASM.DivS
getTwoStackOp (AST.Rot   _)   = ASM.RotS
getTwoStackOp (AST.Mod   _)   = ASM.ModS
getTwoStackOp (AST.DotPr _)   = ASM.DotPr


-- repeat n times:
-- friggin ruby in haskell dude
times = replicateM_

typeSize :: AST.Type -> Int
typeSize AST.Scalar = 8
typeSize (AST.Vector n) = fromIntegral n
