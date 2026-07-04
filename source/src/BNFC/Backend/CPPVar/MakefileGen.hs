{-|
  Module      : BNFC.Backend.CPPVar.MakefileGen
  Description : Makefile generator.
  
  Makefile generator.
-}

module BNFC.Backend.CPPVar.MakefileGen
  (
    -- * The entrypoint
    makeMakefile
  ) where

import Text.PrettyPrint (Doc, text, ($+$))
import qualified BNFC.Options as Options

import BNFC.Backend.CPPVar.CPPUtil

-- | Generates an example Makefile to compile the parser.
makeMakefile ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> Doc
makeMakefile opts =
  variables langname
  $++$ helpTextVariable langname
  $++$ rules langname
  where
    langname = Options.lang opts

------------------------------------------------------------------------
-- * Utility.
------------------------------------------------------------------------

-- | Generates a table of available targets with descriptions.
-- Allows to specify the (minimal) width.
makeHelpTable ::
     Int                 -- ^ The minimal width.
  -> [(String, String)]  -- ^ Target names and descriptions.
  -> Doc
makeHelpTable minWidth rows =
  text (makeRow sTarget sDescription)
  $+$ text (replicate (leftColSize + 1) '-'
      ++ ('|' : replicate (rightColSize + 1) '-'))
  $+$ linesToText (map (uncurry makeRow) rows)
  where
    sTarget :: String
    sTarget = "TARGET"
    sDescription :: String
    sDescription = "DESCRIPTION"
    leftColSize = foldr max (length sTarget) $ map (length . fst) rows
    rightColSizeUnadjusted = foldr max (length sDescription)
      $ map (length . snd) rows
    width = max minWidth (leftColSize + rightColSizeUnadjusted + 3)
    rightColSize = width - leftColSize - 3
    makeRow target desc = concat
      [target, replicate (leftColSize - length target) ' ', " | " , desc]

-- | Generates a table of available targets with descriptions.
-- The table will be at least 80 characters wide.
makeHelpTable80 :: [(String, String)] -> Doc
makeHelpTable80 = makeHelpTable 80 

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

-- | Variable definitions.
variables ::
     String  -- ^ Language name.
  -> Doc
variables langname = linesToText
  [ "CXXFLAGS = -std=c++17 -Wall -Wextra -Wno-unused-but-set-variable"
  , ""
  , "OBJECTS = Absyn.o \\"
  , "\tPrettyPrinter.o \\"
  , "\tPrinterCommon.o \\"
  , "\tSyntaxPrinter.o \\"
  , "\tTest.o \\"
  , "\t" ++ langname ++ ".lex.o \\"
  , "\t" ++ langname ++ ".tab.o"
  , ""
  , "ARCHIVES = lib" ++ langname ++ "Parser.a lib" ++ langname ++ "Printer.a"
  , ""
  , "BNFC_GENERATED = Absyn.cpp \\"
  , "\tAbsyn.hpp \\"
  , "\tPatternMatching.hpp \\"
  , "\tPrettyPrinter.cpp \\"
  , "\tPrettyPrinter.hpp \\"
  , "\tPrinterCommon.cpp \\"
  , "\tPrinterCommon.hpp \\"
  , "\tSyntaxPrinter.cpp \\"
  , "\tSyntaxPrinter.hpp \\"
  , "\tTest.cpp \\"
  , "\t" ++ langname ++ ".l \\"
  , "\t" ++ langname ++ ".ypp"
  ]

-- | A help table for phony targets.
phonyHelp :: Doc
phonyHelp = makeHelpTable80
  [ ("help", "Print this message.")
  , ("default", "Build static libraries for the parser and the printers.")
  , ("all", "Build the static libraries and the \"Test\" program.")
  , ("mostlyclean", "Delete all object/executable files.")
  , ("clean", "Delete all files created by this Makefile and BNFC backups.")
  ]

-- | A help table for file targets.
nonPhonyHelp :: String -> Doc
nonPhonyHelp langname = makeHelpTable80
  [ ("Test", "A program that performs parsing and prints the AST.")
  , ("lib" ++ langname ++ "Parser.a",
    "Implements the syntax tree, lexing, and parsing.")
  , ("lib" ++ langname ++ "Printer.a",
    "Implements pretty-, syntax-tree-, and linear-printing.")
  , (langname ++ ".lex.cpp", "Contains a lexer implementation.")
  , (langname ++ ".tab.{h,c}pp", "Contain a syntax parser implementation.")
  ]

-- | Definition of the @HELPMESSAGE@ variable. Contains the help text.
helpTextVariable :: String -> Doc
helpTextVariable langname = text "define HELPMESSAGE"
  $+$ text "Phony targets:" $++$ phonyHelp
  $++$ text "File targets:" $++$ nonPhonyHelp langname
  $++$ text "For any *.cpp file, a corresponding *.o file can be built."
  $+$ text "endef"

-- | All Makefile rules.
rules ::
     String  -- ^ Language name.
  -> Doc
rules langname = linesToText
  [ ".PHONY: all clean mostlyclean default help"
  , ""
  , "default: $(ARCHIVES)"
  , ""
  , "help:"
  , "\t$(file >/dev/stdout,$(HELPMESSAGE))"
  , "\t@:"
  , ""
  , "all: $(ARCHIVES) Test"
  , ""
  , "mostlyclean:"
  , "\t@for f in $(OBJECTS) $(ARCHIVES) Test; do test ! -f $$f || rm $$f; done"
  , ""
  , "clean: mostlyclean"
  , "\t@ for f in " ++ langname ++ ".lex.cpp " ++ langname ++ ".tab.cpp "
    ++ langname ++ ".tab.hpp; do \\"
  , "\t\ttest ! -f $$f || rm $$f; \\"
  , "\tdone"
  , "\t@ for f in $(BNFC_GENERATED); do \\"
  , "\t\ttest ! -f $$f.bak || rm $$f.bak; \\"
  , "\tdone"
  , ""
  , langname ++ ".lex.cpp: " ++ langname ++ ".l"
  , "\tflex " ++ langname ++ ".l"
  , ""
  , langname ++ ".tab.hpp " ++ langname ++ ".tab.cpp &: " ++ langname ++ ".ypp"
  , "\tbison " ++ langname ++ ".ypp"
  , ""
  , "CXX_COMPILE = $(CXX) $(CXXFLAGS) -c -o $@ $<"
  , "Absyn.o: Absyn.cpp Absyn.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , langname ++ ".tab.o: " ++ langname ++ ".tab.cpp "
    ++ langname ++ ".tab.hpp \\"
  , "\tAbsyn.hpp PatternMatching.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , langname ++ ".lex.o: " ++ langname ++ ".lex.cpp " ++ langname ++ ".tab.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "PrinterCommon.o: PrinterCommon.cpp PrinterCommon.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "PrettyPrinter.o: PrettyPrinter.cpp PrettyPrinter.hpp \\"
  , "\tAbsyn.hpp PrinterCommon.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "SyntaxPrinter.o: SyntaxPrinter.cpp SyntaxPrinter.hpp \\"
  , "\tAbsyn.hpp PrinterCommon.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "AR_COMPILE = $(AR) $(ARFLAGS) $@ $^"
  , "lib" ++ langname ++ "Parser.a: Absyn.o " ++ langname ++ ".lex.o "
    ++ langname ++ ".tab.o"
  , "\t$(AR_COMPILE)"
  , ""
  , "lib" ++ langname ++ "Printer.a: PrinterCommon.o PrettyPrinter.o "
    ++ "SyntaxPrinter.o"
  , "\t$(AR_COMPILE)"
  , ""
  , "Test.o: Test.cpp Absyn.hpp " ++ langname ++ ".tab.hpp \\"
  , "\tPrettyPrinter.hpp SyntaxPrinter.hpp PatternMatching.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "Test: Test.o lib" ++ langname ++ "Parser.a lib" ++ langname ++ "Printer.a"
  , "\t$(CXX) $(LDFLAGS) $^ -o Test"
  ]
