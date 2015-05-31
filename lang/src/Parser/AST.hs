module Parser.AST where

data Expr = Lit Integer	
          | VecLit [Integer]
          | VarE Var
          | BinOp Op Expr Expr
          | If Expr [Expr] [Expr]
          | Assign Var Expr
          | Decl Var Type Expr
          deriving Show

type Var = String
data Type = Scalar | Vector Integer deriving Show


data Op = Add | Sub | Mul | Div deriving Show
