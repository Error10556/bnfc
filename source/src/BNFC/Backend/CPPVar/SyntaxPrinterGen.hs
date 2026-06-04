module BNFC.Backend.CPPVar.SyntaxPrinterGen
    ( syntaxPrinterHppFilename
    , syntaxPrinterCppFilename
    , makeSyntaxPrinter) where

import BNFC.Backend.CPPVar.CPPUtil
import qualified BNFC.CF
import qualified BNFC.Options
import Text.PrettyPrint

syntaxPrinterHppFilename :: String
syntaxPrinterHppFilename = "SyntaxPrinter.hpp"

syntaxPrinterCppFilename :: String
syntaxPrinterCppFilename = "SyntaxPrinter.cpp"

-- | -> (hpp, cpp)
makeSyntaxPrinter :: BNFC.Options.SharedOptions -> BNFC.CF.CF -> (Doc, Doc)
makeSyntaxPrinter opts cf = (hpp, cpp)
    where
        hpp = linesToText
            [ "#include <iostream>"
            , "#include <string_view>"
            , "#include \"Absyn.hpp\""
            ] $++$ packwrap
            (empty)

        cpp = empty
        packwrap = wrapPackage opts
