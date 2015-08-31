module Parser.AST where

import Data.Default
import Data.Int      (Int16)

type Module = [Expr]

data Expr = Lit Int16
          | VecLit [Int16]
          | VarE Var
          | BinOp Op Expr Expr
          | If Expr [Expr] [Expr]
          | Assign Var Expr
          | Decl Var Type Expr
          deriving (Show, Eq, Ord)

type Var = String
data Type = Scalar | Vector Int16 deriving (Show, Eq, Ord)


data Op = Add Type | Sub Type | Mul Type | Div Type deriving (Show, Eq, Ord)

instance Default Type where
    def = Scalar
