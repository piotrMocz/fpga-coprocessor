module Main where


import Parser.Parser       (parseFile)
import CodeGen.Generator   (symTable, modAST, asmCode, runASTTranslation, avAddrs)
import CodeGen.ASM2        (makeASM)
import CodeGen.LabelRename (renameLabels)
import CodeGen.Typechecker (runTypechecker)

import Text.Show.Pretty  (ppShow)
import Control.Lens

main :: IO ()
main = do
    result <- parseFile "example"
    print "=========== AST ================"
    putStrLn . ppShow $ result
    print "========= Typechecking ========="
    putStrLn . ppShow $ runTypechecker . head $ result
    print "=========== ASMGen ============="
    let genData = runASTTranslation result
    putStrLn . show $ genData ^. symTable
    putStrLn . show $ genData ^. avAddrs
    putStrLn . show $ genData ^. asmCode

    	-- asm   = genData ^. asmCode
      --   code1 = makeASM asm
      --   asm2  = renameLabels asm
      --   code2 = makeASM asm2
    -- print "=========== ASM generation ============="
    -- putStrLn code1
    -- print "=========== Label renaming ============="
    -- putStrLn code2
