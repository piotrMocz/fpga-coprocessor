-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE FlexibleContexts #-}
module Parser.Lexer where

import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)
import Text.Parser.Char (oneOf, alphaNum, char)
import Text.Parser.Combinators (many, choice, notFollowedBy)
import Text.Parser.Token (TokenParsing, integer', symbol)
import qualified Parser.AST as A


integer :: TokenParsing m => m Integer
integer = integer' <* whitespaces

variable :: (TokenParsing m, Monad m) => m String
variable = (notFollowedBy $ choice [ifL, thenL, endL, elseL]) *> ((:) <$> (oneOf ['a'..'z'])  <*> many alphaNum <* whitespaces)

equals :: TokenParsing m => m Char
equals = char '=' <* whitespaces


addop, mulop :: TokenParsing m => m (A.Expr -> A.Expr -> A.Expr)
addop = choice [char '+' *> whitespaces *> pure (A.BinOp A.Add), char '-' *> whitespaces *> pure (A.BinOp A.Sub)]
mulop = choice [char '*' *> whitespaces *> pure (A.BinOp A.Mul), char '/' *> whitespaces *> pure (A.BinOp A.Div)]


whitespaces :: TokenParsing m => m String
whitespaces = many $ oneOf "\t "

newline :: TokenParsing m => m String
newline = many $ oneOf "\n;"

ifL :: TokenParsing m => m String
ifL = symbol "if" <* whitespaces

thenL :: TokenParsing m => m String
thenL = symbol "then" <* whitespaces

elseL :: TokenParsing m => m String
elseL = symbol "else" <* whitespaces

endL :: TokenParsing m => m String
endL = symbol "end" <* whitespaces
