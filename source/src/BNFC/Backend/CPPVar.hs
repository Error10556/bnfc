{-
    BNF Converter -> C++17 with std::variant
    (C) (2026) Author: Timur Usmanov <t.usmanov@innopolis.university>
-}

module BNFC.Backend.CPPVar (makeCppVar) where

import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import qualified BNFC.Backend.CPPVar.CPPUtil as CPPUtil
import BNFC.Backend.CPPVar.AbsynGen
import BNFC.Backend.CPPVar.FlexGen
import BNFC.Backend.CPPVar.BisonGen
import BNFC.Backend.CPPVar.PatternMatchingGen
import BNFC.Backend.CPPVar.PrinterUtils
import BNFC.Backend.CPPVar.SyntaxPrinterGen
import BNFC.Backend.CPPVar.PrettyPrinterGen
import BNFC.Backend.CPPVar.TestGen

comment :: String -> String
comment = ("/* "++) . (++" */")

makeCppVar :: SharedOptions -> CF -> MkFiles ()
makeCppVar opts cf = do
    let groupedRules = case CPPUtil.groupRules cf of
            Left msg -> error msg
            Right val -> val
        (absynHpp, absynCpp) = makeAbsyn opts cf groupedRules
        (flexFile, implicitTokenNames) = makeFlex opts cf
        bisonFile = makeBison opts cf implicitTokenNames groupedRules
        printables = getPrintableSymbols cf groupedRules
        (syntaxHpp, syntaxCpp) = makeSyntaxPrinter opts printables
        (prettyHpp, prettyCpp) = makePrettyPrinter opts printables
    mkfile absynHppFilename comment absynHpp
    mkfile absynCppFilename comment absynCpp
    mkfile (flexFilename opts) comment flexFile
    mkfile (bisonFilename opts) comment bisonFile
    mkfile patternMatchingFilename comment patternMatchingHpp
    mkfile syntaxPrinterHppFilename comment syntaxHpp
    mkfile syntaxPrinterCppFilename comment syntaxCpp
    mkfile prettyPrinterHppFilename comment prettyHpp
    mkfile prettyPrinterCppFilename comment prettyCpp
    mkfile testFilename comment makeTest
