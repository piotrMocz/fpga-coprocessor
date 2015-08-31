module Parser.AST where

import Data.Default


type Module = [Expr]


data Expr = Lit Integer
          | VecLit [Integer]
          | VarE VarName
          | BinOp Op Expr Expr
          | If Expr [Expr] [Expr]
          | Assign VarName Expr
          | Decl VarName Type Expr
          deriving (Show, Eq, Ord)


type VarName = String
data Type    = Scalar | Vector Integer deriving (Show, Eq, Ord)


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