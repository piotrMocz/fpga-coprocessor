module Parser.AST where

data Expr = Lit Integer	
          | VarE Var
          | BinOp Op Expr Expr
          | If Expr [Expr] [Expr]
          | Assign Var Expr
          | Decl Var Type Expr
          | Unit
          deriving Show

type Var = String
type Type = String


data Op = Add | Sub | Mul | Div deriving Show
