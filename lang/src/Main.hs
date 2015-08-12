module Main where


import Parser.Parser     (parseFile)
import CodeGen.Generator (asmCode, runASTTranslation)
import CodeGen.ASM2      (makeASM)

import Text.Show.Pretty  (ppShow)
import Control.Lens	

main :: IO ()
main = do
    result <- parseFile "example"
    putStrLn . ppShow $ result
    let genData = runASTTranslation result
    	asm = genData ^. asmCode
    putStrLn . makeASM $ asm
