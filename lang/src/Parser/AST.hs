-- Introducing static types

module Parser.AST where

data Expr = Lit Int
          | VarE Var
          | BinOp Op Expr Expr
          | If Expr [Expr] [Expr]
          | Assign Var Expr
          | Decl Var Type Expr
          | Unit
          deriving Show

-- Op is defined in MIPS.hs as follows:
data Op = Add | Sub | Mul | Div deriving Show


--data Stm = Decl String String Expr
--         | Expr Expr
--         deriving Show

