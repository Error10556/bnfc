{-|
  Module      : BNFC.Backend.CPPVar
  Description : C++17 backend using FLex, Bison, and the standard template
                library (especially std::variant).

  C++17 backend using FLex, Bison, and the standard template library
  (especially std::variant).
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
import qualified BNFC.Backend.CPPVar.AbsynGen as AbsynGen
import qualified BNFC.Backend.CPPVar.FlexGen as FlexGen
import qualified BNFC.Backend.CPPVar.BisonGen as BisonGen
import qualified BNFC.Backend.CPPVar.PatternMatchingGen as PatternMatchingGen
import qualified BNFC.Backend.CPPVar.PrinterUtils as PrinterUtils
import qualified BNFC.Backend.CPPVar.SyntaxPrinterGen as SyntaxPrinterGen
import qualified BNFC.Backend.CPPVar.PrettyPrinterGen as PrettyPrinterGen
import qualified BNFC.Backend.CPPVar.TestGen as TestGen
import qualified BNFC.Backend.CPPVar.PrinterCommonGen as PrinterCommonGen
import qualified BNFC.Backend.CPPVar.MakefileGen as MakefileGen

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
makeCppVar opts (CFG
  { cfgPragmas        = cfPragmas
  -- , cfgUsedCats       = cfUsedCats
  , cfgLiterals       = cfLiterals
  , cfgSymbols        = cfSymbols
  , cfgKeywords       = cfKeywords
  , cfgRules          = cfRules
  }) = do
  let
    groupedRules       = CPPUtil.groupRules cfRules
    mergedGroupedRules = CPPUtil.mergeCoercCats groupedRules
    cfTerminals = cfSymbols ++ cfKeywords
    AbsynGen.GeneratedAbsyn
      { absynCode = CPPUtil.CPPHeaderSourcePair
        { cppHeaderText = absynHpp
        , cppSourceText = absynCpp
        }
      , absynListItemStorage = listItemStorage
      } = AbsynGen.makeAbsyn opts cfLiterals cfPragmas mergedGroupedRules
    FlexGen.CompiledLexer
      { compiledLexer_flexGrammar        = flexFile
      , compiledLexer_implicitTokenNames = implicitTokenNames
      } = FlexGen.makeFlex opts cfLiterals cfTerminals cfPragmas
    bisonFile = BisonGen.makeBison
      opts implicitTokenNames cfLiterals cfPragmas groupedRules listItemStorage
    printables = PrinterUtils.getPrintableSymbols
      cfLiterals cfPragmas mergedGroupedRules
    CPPUtil.CPPHeaderSourcePair
      { cppHeaderText = syntaxHpp
      , cppSourceText = syntaxCpp
      } = SyntaxPrinterGen.makeSyntaxPrinter opts printables
    CPPUtil.CPPHeaderSourcePair
      { cppHeaderText = prettyHpp
      , cppSourceText = prettyCpp
      } = PrettyPrinterGen.makePrettyPrinter opts printables
  mkfile AbsynGen.absynHppFilename comment absynHpp
  mkfile AbsynGen.absynCppFilename comment absynCpp
  mkfile (FlexGen.flexFilename opts) comment flexFile
  mkfile (BisonGen.bisonFilename opts) comment bisonFile
  mkfile PatternMatchingGen.patternMatchingFilename comment
    PatternMatchingGen.patternMatchingHpp
  mkfile PrinterCommonGen.printerCommonHppFilename comment
    $ PrinterCommonGen.makePrinterCommonHpp opts
  mkfile PrinterCommonGen.printerCommonCppFilename comment
    $ PrinterCommonGen.makePrinterCommonCpp opts
  mkfile SyntaxPrinterGen.syntaxPrinterHppFilename comment syntaxHpp
  mkfile SyntaxPrinterGen.syntaxPrinterCppFilename comment syntaxCpp
  mkfile PrettyPrinterGen.prettyPrinterHppFilename comment prettyHpp
  mkfile PrettyPrinterGen.prettyPrinterCppFilename comment prettyCpp
  mkfile TestGen.testFilename comment (TestGen.makeTest opts)
  case optMake opts of
    Nothing           -> return ()
    Just makefileName -> mkfile makefileName ("# " ++)
      $ MakefileGen.makeMakefile opts

-- | C++ comment wrapper.
comment :: String -> String
comment = ("/* " ++) . (++ " */")
