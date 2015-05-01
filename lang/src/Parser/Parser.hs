module Parser.Parser where

import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)
import Control.Monad.IO.Class (MonadIO)
import Text.Parser.Token (TokenParsing)
import Text.Trifecta.Parser (parseFromFile, Parser)
import Text.Parser.Combinators (chainl1, choice, option, many)

import qualified Parser.AST   as A
import qualified Parser.Lexer as L


-- ------------------
-- -- StatementParser
-- ------------------

pStm :: Parser A.Stm
pStm = A.Expr <$> pExpr <* L.newline

-- ---------------------
-- Expression Parser --
-- ---------------------
pExpr :: Parser A.Expr
pExpr = L.whitespaces *> choice [pIf, pBinOp]

pIf :: Parser A.Expr
pIf = do
       L.ifL
       cond <- pExpr
       L.thenL
       trueCase <- many pStm
       falseCase <- option [] parseElse
       L.endL
       return $ A.If cond trueCase falseCase
    where 
       parseElse = do
       	  L.elseL
       	  many pStm

pLit :: Parser A.Expr
pLit = A.Lit <$> L.integer

pVar :: Parser A.Expr
pVar = A.Var <$> L.variable

pAssign :: Parser A.Expr
pAssign = A.Assign <$> L.variable <* L.equals <*> pExpr

pBinOp :: Parser A.Expr
pBinOp = term `chainl1` L.addop
term = factor `chainl1` L.mulop
factor = choice [pLit, pVar] 


main ::  MonadIO m => m (Maybe A.Stm)
main = parseFromFile pStm "example"



-- --------------------
-- -- Program parser --
-- --------------------

-- prog :: Parser [A.Stm]
-- prog =  stmt

-- varList :: Parser [String]
-- varList = sepBy L.identifier L.comma

-- exprList :: Parser [A.Expr]
-- exprList = sepBy pExpr L.comma




-- stmt :: Parser [A.Stm]
-- stmt = sepEndBy table (L.reservedOp ";")
--     where 
--       table = do  { 
--                 e <- pExpr;
--                 return . A.Expr $ e;
--               }
--        <|> do { L.reserved "if"; cond <- pExpr;
--                 L.reserved "then"; s1 <- stmt;
--                 L.reserved "else"; s2 <- stmt;
--                 return $ A.If cond s1 s2}
--        <|> do { L.reserved "while"; cond <- pExpr; s <- stmt;
--                return $ A.While cond s
--               }




-- parse input =
--   case runParser p () "" input of
--     Left err -> error ("parsing " ++ show err)
--     Right s -> s
--   where
--     p =   L.integer

-- parseFile :: String -> IO ()
-- parseFile filename = do 
--           input <- readFile filename; 
--           putStrLn . show $ parse input 

-- utilfun2 '\n' = ';'
-- utilfun2 x = x
