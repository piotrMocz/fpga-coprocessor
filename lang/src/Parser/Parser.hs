module Parser.Parser where

import Control.Monad
import System.IO (readFile)
import Text.ParserCombinators.Parsec (parse, Parser, sepBy1, sepBy, (<|>), try, between)
import Text.ParserCombinators.Parsec.Expr (Operator(Infix), Assoc(AssocLeft), buildExpressionParser)
import Text.ParserCombinators.Parsec (char)
import Text.Show.Pretty (ppShow)


import qualified Parser.AST   as A
import           Data.Default   (def)
import qualified Parser.Lexer as L

progParser :: Parser A.Module
progParser = L.whiteSpace >> statement

statement :: Parser [A.Expr]
statement = sepBy1 aExpression L.whiteSpace

pType :: Parser A.Type
pType = try vectorType <|> try intType

intType :: Parser A.Type
intType =
    do L.reserved "Int"
       return A.Scalar

vectorType :: Parser A.Type
vectorType =
    do L.reserved "IntVector"
       L.reservedOp "["
       size <- L.integer
       L.reservedOp "]"
       return . A.Vector . fromInteger $ size

pVecLit :: Parser A.Expr
pVecLit =
    do L.reservedOp "["
       numbers <- sepBy L.integer (L.reservedOp ",")
       L.reserved "]"
       return . A.VecLit $ map fromInteger numbers

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

loopStmt :: Parser A.Expr
loopStmt =
    do L.reserved "loop"
       L.reservedOp "("
       expr <- aExpression
       L.reserved ")"
       L.reservedOp ":"
       stmt <- statement
       L.reserved "end"
       return $ A.Loop expr stmt

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
       tp <- pType
       L.reservedOp "="
       expr <- aExpression
       return $ A.Decl var tp expr

aExpression :: Parser A.Expr
aExpression = buildExpressionParser aOperators aTerm


aOperators = [ [Infix  (L.reservedOp "*"   >> return (A.BinOp (A.Mul def))) AssocLeft,
                Infix  (L.reservedOp "/"   >> return (A.BinOp (A.Div def))) AssocLeft]
             , [Infix  (L.reservedOp "+"   >> return (A.BinOp (A.Add def))) AssocLeft,
                Infix  (L.reservedOp "-"   >> return (A.BinOp (A.Sub def))) AssocLeft]
              ]


aTerm =  L.parens aExpression
     <|> try pVecLit
     <|> try assignStmt
     <|> try declStmt
     <|> try ifStmt
     <|> try loopStmt
     <|> liftM A.VarE L.identifier
     <|> liftM (A.Lit . fromInteger) L.integer



parseString :: String -> [A.Expr]
parseString str =
  case parse progParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO A.Module
parseFile file =
  do program  <- readFile file
     return $ case parse progParser "" program of
                Left e  -> []
                Right r -> r
