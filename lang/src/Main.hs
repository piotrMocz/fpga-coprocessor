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
    putStrLn . ppShow $ runTypechecker result
    print "=========== ASMGen ============="
    let genData = runASTTranslation result
    putStrLn .               show . view symTable $ genData
    putStrLn .               show . view avAddrs  $ genData
    putStrLn . unlines . map show . view asmCode  $ genData

    -- print "=========== ASM generation ============="
    -- putStrLn code1
    -- print "=========== Label renaming ============="
    -- putStrLn code2
