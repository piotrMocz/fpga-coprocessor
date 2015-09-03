module Parser.AST where

import Data.Default
import Data.Int      (Int16)


type Module = [Expr]

data Expr = Lit Int16
          | VecLit [Int16]
          | VarE VarName
          | BinOp Op Expr Expr
          | If Expr [Expr] [Expr]
          | Assign VarName Expr
          | Decl VarName Type Expr
          | Loop Expr [Expr]
          deriving (Show, Eq, Ord)


type VarName = String
data Type    = Scalar | Vector Int16 deriving (Show, Eq, Ord)


data Op = Add Type | Sub Type | Mul Type | Div Type deriving (Show, Eq, Ord)


instance Default Type where
    def = Scalar


------- UTILS --------
loopIf :: Int -> Int -> (Expr, Expr, Expr)
loopIf lab reps =
    ( Decl varName Scalar (Lit . fromIntegral $ reps + 1)        -- counter
    , Assign varName (BinOp (Sub Scalar) (VarE varName) (Lit 1)) -- decrement counter
    , VarE varName                                               -- evaluate counter
    ) where varName = "counter" ++ show lab
