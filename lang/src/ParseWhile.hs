module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Show.Pretty (ppShow)


data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show)


data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If AExpr Stmt Stmt
          | While AExpr Stmt
          | Skip
            deriving (Show)

languageDef =
  emptyDef { Token.commentStart    = "{#"
           , Token.commentEnd      = "#}"
           , Token.commentLine     = "#"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "end"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "="]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

progParser :: Parser Stmt
progParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' whiteSpace)
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- aExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
 


assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp "="
     expr <- aExpression
     return $ Assign var expr


aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm


aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]
 

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer


parseString :: String -> Stmt
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