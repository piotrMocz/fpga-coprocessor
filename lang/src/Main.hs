module Main where

import CodeGen.LabelRename (renameLabels)
import Parser.Parser       (parseFile)
import CodeGen.Generator   (symTable, modAST, asmCode, runASTTranslation, avAddrs, constData)
import CodeGen.ASM         (makeASM)
import CodeGen.Typechecker (runTypechecker, TypecheckerError, err)
import CodeGen.Binary      (genBinary)


import Text.Show.Pretty  (ppShow)
import Control.Lens
import Control.Monad     ((<=<))


runCompiler ast = case runTypechecker ast of
    Right newAst -> runASTTranslation newAst
    Left  tcerr  -> error $ "Typechecker error:\n    " ++ tcerr ^. err


main :: IO ()
main = do
    print "==========Parsing==============="
    result <- parseFile "example"
    print "=========ASTPrinting============"
    putStrLn . ppShow $ result
    print "=========== ASMGen ============="
    let genData = runCompiler result
    print "========Proper labels==========="
    let asm = renameLabels (view asmCode genData)
    let consts = view constData genData
    print "======Code and const data======="
    putStrLn . unlines . map show $ asm
    putStrLn . unlines . map show $ consts
    print "=========Saving binary=========="
    genBinary "binarka" (fmap snd asm) consts
