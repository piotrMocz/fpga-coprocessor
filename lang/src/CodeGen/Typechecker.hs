{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}

module CodeGen.Typechecker where

import           Parser.AST                (Type(..), VarName, Expr(..), Op(..))

import qualified Data.Map                   as MP
import           Control.Lens
import           Control.Monad.Trans.Except
import           Control.Monad.State
import           Data.Monoid               ((<>))
import           Control.Monad             (liftM)
import           Data.Either.Combinators   (fromRight)


type TypeMap = MP.Map VarName Type

newtype TypecheckerError = TypecheckerError { _err :: String } deriving (Show, Eq, Ord)
makeLenses ''TypecheckerError

type TypeState a = ExceptT TypecheckerError (State TypeMap) a

-- runTypechecker :: Expr -> Expr
runTypechecker e = evalState (runExceptT $ typeCheck e) MP.empty
-- runTypechecker e = do
--     case runExceptT $ typeCheck e of
--         Left l -> putStrLn . show $ l
--         Right r -> putStrLn . show $ r


typeCheck :: Expr -> TypeState Expr
typeCheck (Lit a) = return . Lit $ a
typeCheck (VecLit a) = return . VecLit $ a
typeCheck (VarE var) = return $ VarE var
typeCheck (Assign nm expr) = do
    e <- typeCheck expr
    tp <- infer e
    updateExistingValue nm tp
    return $ Assign nm e
typeCheck (Decl nm tp expr) = do
    e <- typeCheck expr
    exprTp <- infer expr
    if tp /= exprTp then throwE . TypecheckerError $ "Variable" <> nm <> "already declared."
                else do {updateExistingValue nm tp; return $ Decl nm tp e}

typeCheck (BinOp op expr1 expr2) = do
    e1 <- typeCheck expr1
    e2 <- typeCheck expr2
    t1 <- infer e1
    t2 <- infer e2
    if t1 /= t2
      then throwE . TypecheckerError $ show t1 <> "and" <> show t2 <> "differ."
      else case op of
              Mul _ -> return (BinOp (Mul Scalar) e1 e2 )
              Add x -> return (BinOp (Add t1) e1 e2 )
              Sub x -> return (BinOp (Sub t1) e1 e2 )
              Div x -> return (BinOp (Div t1) e1 e2 )




infer :: Expr -> TypeState Type
infer (Lit _) = return Scalar
infer (VecLit a) = return $ Vector $ toInteger . length $ a
infer (VarE var) = liftM (MP.lookup var) get
                 >>= \case
                     Nothing -> throwE . TypecheckerError $ "Variable" <> var <> "undeclared."
                     Just tp -> return tp
infer (Assign nm expr) = do
    e <- infer expr
    return e

infer (Decl _ _ expr) = do
    e <- infer expr
    return e

infer (BinOp op expr1 expr2) = do
    tp1 <- infer expr1
    tp2 <- infer expr2
    if tp1 /= tp2
      then throwE . TypecheckerError $ show tp1 <> "and" <> show tp2 <> "differ."
      else case op of
              Mul _ -> return Scalar
              Add x -> return x
              Sub x -> return x
              Div x -> return x


-- infer (BinOp op expr1 expr2)




insertNewValue :: VarName -> Type -> TypeState ()
insertNewValue nm tp = do
    mp <- get
    case MP.lookup nm mp of
        Nothing -> put $ MP.insert nm tp mp
        Just _ -> throwE . TypecheckerError $ "Variable" <> nm <> "already declared."


updateExistingValue :: VarName -> Type -> TypeState ()
updateExistingValue nm tp = do
    mp <- get
    case MP.lookup nm mp of
        Nothing -> throwE . TypecheckerError $ "Variable" <> nm <> "undeclared."
        Just a -> if a == tp then put $ MP.insert nm tp mp else throwE . TypecheckerError $ "Cannot assign" <> show tp <> "to" <> nm <> "of type" <> show a
