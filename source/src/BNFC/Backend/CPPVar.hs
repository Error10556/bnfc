{-|
  Module      : BNFC.Backend.CPPVar
  Description : C++17 backend using FLex, Bison, and the standard template
                library (especially std::variant).
  Maintainer  : Timur Usmanov <t.usmanov@innopolis.university>
-}

module BNFC.Backend.CPPVar
  (
    -- * The entrypoint
    makeCppVar
  ) where

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
import BNFC.Backend.CPPVar.PrinterCommonGen
import BNFC.Backend.CPPVar.MakefileGen

{-| Generates the following files (@language@ is the basename of the LBNF
grammar description file, as given by 'BNFC.Options.lang'):

+----------------------------+-------------------------------------------+
|            FILE            |                DESCRIPTION                |
+============================+===========================================+
| @Absyn.hpp@                | abstract syntax nodes declarations        |
+----------------------------+-------------------------------------------+
| @PatternMatching.hpp@      | the pipe operator overload                |
+----------------------------+-------------------------------------------+
| @PrinterCommon.hpp@        | common string visualization utilities     |
+----------------------------+-------------------------------------------+
| @PrettyPrinter.hpp@        | pretty-printing                           |
+----------------------------+-------------------------------------------+
| @SyntaxPrinter.hpp@        | visualization of the abstract syntax tree |
+----------------------------+-------------------------------------------+
| @language.l@               | the FLex lexer definition                 |
+----------------------------+-------------------------------------------+
| @language.ypp@             | the Bison parser definition               |
+----------------------------+-------------------------------------------+
| @Makefile@ (if @-m@ given) | recipes reference                         |
+----------------------------+-------------------------------------------+
| @Absyn.cpp@                |                                           |
+----------------------------+-------------------------------------------+
| @PrinterCommon.cpp@        |                                           |
+----------------------------+-------------------------------------------+
| @PrettyPrinter.cpp@        |                                           |
+----------------------------+-------------------------------------------+
| @SyntaxPrinter.cpp@        |                                           |
+----------------------------+-------------------------------------------+
| @Test.cpp@                 | an example parser, used for testing       |
+----------------------------+-------------------------------------------+
-}

makeCppVar ::
     SharedOptions  -- ^ BNFC invokation options.
  -> CF             -- ^ The grammar description.
  -> MkFiles ()
makeCppVar opts cf = do
  let
    groupedRules = CPPUtil.groupRules cf
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
  mkfile printerCommonHppFilename comment (makePrinterCommonHpp opts)
  mkfile printerCommonCppFilename comment (makePrinterCommonCpp opts)
  mkfile syntaxPrinterHppFilename comment syntaxHpp
  mkfile syntaxPrinterCppFilename comment syntaxCpp
  mkfile prettyPrinterHppFilename comment prettyHpp
  mkfile prettyPrinterCppFilename comment prettyCpp
  mkfile testFilename comment (makeTest opts)
  case optMake opts of
    Nothing           -> return ()
    Just makefileName -> mkfile makefileName ("# " ++) (makeMakefile opts)

-- | C++ comment wrapper.
comment :: String -> String
comment = ("/* " ++) . (++ " */")
