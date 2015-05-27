module Parser.Parser where

import Control.Monad
import System.IO (readFile)
import Text.ParserCombinators.Parsec (parse, Parser, sepBy1, (<|>), try)
import Text.ParserCombinators.Parsec.Expr (Operator(Infix), Assoc(AssocLeft), buildExpressionParser)
import Text.Show.Pretty (ppShow)


import qualified Parser.AST   as A
import qualified Parser.Lexer as L

progParser :: Parser [A.Expr]
progParser = L.whiteSpace >> statement

statement :: Parser [A.Expr]
statement = sepBy1 aExpression L.whiteSpace




ifStmt :: Parser A.Expr
ifStmt =
  do L.reserved "if"
     cond  <- aExpression
     L.reserved "then"
     stmt1 <- statement
     L.reserved "else"
     stmt2 <- statement
     L.reserved "end"
     return $ A.If cond stmt1 stmt2
 


assignStmt :: Parser A.Expr
assignStmt =
  do var  <- L.identifier
     L.reservedOp "="
     expr <- aExpression
     return $ A.Assign var expr

declStmt :: Parser A.Expr
declStmt =
    do var <- L.identifier
       L.reservedOp ":"
       tp <- L.identifier
       L.reservedOp "="
       expr <- aExpression
       return $ A.Decl var tp expr

aExpression :: Parser A.Expr
aExpression = buildExpressionParser aOperators aTerm


aOperators = [ [Infix  (L.reservedOp "*"   >> return (A.BinOp A.Mul)) AssocLeft,
                Infix  (L.reservedOp "/"   >> return (A.BinOp A.Div)) AssocLeft]
             , [Infix  (L.reservedOp "+"   >> return (A.BinOp A.Add)) AssocLeft,
                Infix  (L.reservedOp "-"   >> return (A.BinOp A.Sub)) AssocLeft]
              ]
 

aTerm =  L.parens aExpression
     <|> try assignStmt
     <|> try declStmt
     <|> try ifStmt
     <|> liftM A.VarE L.identifier
     <|> liftM A.Lit L.integer



parseString :: String -> [A.Expr]
parseString str =
  case parse progParser "" str of
    Left e  -> error $ show e
    Right r -> r
 
parseFile :: String -> IO ()
parseFile file =
  do program  <- readFile file
     case parse progParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> putStrLn . ppShow $ r