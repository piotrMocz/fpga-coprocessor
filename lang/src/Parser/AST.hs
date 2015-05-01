-- Introducing static types

module Parser.AST where

data Expr = Lit Integer
          | Var String
          | BinOp Op Expr Expr
          | If Expr [Stm] [Stm]
          | Assign String Expr
          deriving Show

-- Op is defined in MIPS.hs as follows:
data Op = Add | Sub | Mul | Div deriving Show


data Stm = Decl String String Expr
         | Expr Expr
         deriving Show

