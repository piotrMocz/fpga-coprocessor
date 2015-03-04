{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser.Parser where

import Text.Parsec hiding (State, (<|>))
import qualified Text.Parsec.Indent as I
import qualified System.IO (readFile)
import qualified Parser.Lexer as L
import Text.Parsec.String (parseFromFile)
import Control.Monad.State
import Control.Applicative
import Text.Parsec.Expr

source_name :: String
source_name = "example"

main1 :: IO ()
main1 = do 
        input <- readFile source_name
        putStrLn . show $ I.runIndent source_name $ runParserT (I.block pIF) () source_name input





type Name = String

type IParser a = ParsecT String () (State SourcePos) a

--data Block = Block [Instructions]
data IF = IF Expr [Expr] (Maybe [Expr]) deriving Show
data Instruction = Expr Expr | Cond IF
data Expr = Con Int | Var Name | BinExpr Oper Expr Expr | Assignment Name Expr deriving (Show, Read)
data Oper = Add | Sub | Div | Mult deriving (Show, Read)


pExpr :: IParser Expr
pExpr = buildExpressionParser pOperators aTerm

pIF :: IParser IF
pIF = IF <$> (string "if " *> pExpr ) <*> (I.block pExpr) <*> (optionMaybe (string "else" *> spaces *> (I.block pExpr))) 



pOperators = [ [Infix  (char '*' <* spaces  >> return (BinExpr Mult)) AssocLeft]
             , [Infix  (char '/' <* spaces  >> return (BinExpr Div )) AssocLeft]
             , [Infix  (char '+' <* spaces  >> return (BinExpr Add )) AssocLeft]
             , [Infix  (char '-' <* spaces  >> return (BinExpr Sub )) AssocLeft]
             ]

aTerm =  Var <$> L.identifier
     <|> Con <$> L.integer


