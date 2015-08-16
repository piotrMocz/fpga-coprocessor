module Parser.AST where

type Module = [Expr]

data Expr = Lit Integer
          | VecLit [Integer]
          | VarE Var
          | BinOp Op Expr Expr
          | If Expr [Expr] [Expr]
          | Assign Var Expr
          | Decl Var Type Expr
          deriving (Show, Eq, Ord)

type Var = String
data Type = Scalar | Vector Integer deriving (Show, Eq, Ord)


data Op = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

