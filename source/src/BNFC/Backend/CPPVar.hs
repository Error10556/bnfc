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
import BNFC.Backend.CPPVar.SyntaxPrinterGen
import qualified BNFC.Backend.C as BackendC (comment)

makeCppVar :: SharedOptions -> CF -> MkFiles ()
makeCppVar opts cf = do
    let groupedRules = case CPPUtil.groupRules cf of
            Left msg -> error msg
            Right val -> val
        (absynHpp, absynCpp) = makeAbsyn opts cf groupedRules
        (flexFile, implicitTokenNames) = makeFlex opts cf
        bisonFile = makeBison opts cf implicitTokenNames groupedRules
        (syntaxHpp, syntaxCpp) = makeSyntaxPrinter opts cf groupedRules
    mkfile absynHppFilename BackendC.comment absynHpp
    mkfile absynCppFilename BackendC.comment absynCpp
    mkfile (flexFilename opts) BackendC.comment flexFile
    mkfile (bisonFilename opts) BackendC.comment bisonFile
    mkfile patternMatchingFilename BackendC.comment patternMatchingHpp
    mkfile syntaxPrinterHppFilename BackendC.comment syntaxHpp
    mkfile syntaxPrinterCppFilename BackendC.comment syntaxCpp
