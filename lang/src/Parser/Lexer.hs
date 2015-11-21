module Parser.Lexer where

import           Text.ParserCombinators.Parsec          (letter, alphaNum)
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token


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
                                     , "Int"
                                     , "IntVector"
                                     , "loop"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "%", "&", "?", "=", ":", "[", "]", ",", "(", ")"]
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
