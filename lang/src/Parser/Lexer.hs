{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser.Lexer where


import Control.Applicative  ((<$>), (<*>), (*>), (<*))
import Text.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Char (alphaNum)
import Text.Parsec 




eol = char ';' <|> char '\n'

identifier = (:) <$> (oneOf ['a'..'z']) <*> (many alphaNum) <* spaces
integer = (read :: (String -> Int)) <$> (many1 digit) <* spaces
