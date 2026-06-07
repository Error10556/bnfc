module BNFC.Backend.CPPVar.SyntaxPrinterGen
    ( syntaxPrinterHppFilename
    , syntaxPrinterCppFilename
    , makeSyntaxPrinter) where

import BNFC.Backend.CPPVar.CPPUtil
import qualified BNFC.CF
import qualified BNFC.Options
import Text.PrettyPrint
import BNFC.Backend.CPPVar.PrinterUtils

syntaxPrinterHppFilename :: String
syntaxPrinterHppFilename = "SyntaxPrinter.hpp"

syntaxPrinterCppFilename :: String
syntaxPrinterCppFilename = "SyntaxPrinter.cpp"

-- | -> (hpp, cpp)
makeSyntaxPrinter :: BNFC.Options.SharedOptions -> BNFC.CF.CF
    -> GroupedRules -> (Doc, Doc)
makeSyntaxPrinter opts cf groupedRules = (hpp, cpp)
    where
        hpp = linesToText
            [ "#pragma once"
            , "#include <iostream>"
            , "#include <string_view>"
            , "#include \"Absyn.hpp\""
            ] $++$ packwrap (printerClassDecl printable)
        cpp = empty
        packwrap = wrapPackage opts
        printable = getPrintableSymbols cf groupedRules

printerClassDecl :: [PrintableSymbol] -> Doc
printerClassDecl symbols = linesToText
    [ "class SyntaxPrinter {"
    , "    std::ostream& out;"
    , "    bool currentIndentIsBranch;"
    , "    const SyntaxPrinter* maybeParent;"
    , "    SyntaxPrinter(const SyntaxPrinter* parent, " ++
        "bool currentIndentIsBranch);"
    , "    friend const SyntaxPrinter& operator<<(const SyntaxPrinter&,"
    , "                                           std::string_view);"
    , "    void PrintIndentForHeader() const;"
    , "    void PrintIndentAsIs() const;"
    , ""
    , "public:"
    , "    SyntaxPrinter(std::ostream& out);"
    ] $+$ nest 4 (foldr ($+$) empty (map makeMethod symbols))
    $+$ text "};"
    $++$ foldr ($+$) empty (map makeShiftL symbols)
    $+$ makeShiftLRaw "std::string_view"
    where
        makeMethodRaw s = text $ "void operator()(const " ++ s ++ "&) const;"
        makeMethod = \case
            NormalCategory name -> makeMethodRaw name
            ListCategory {printListName = name} -> makeMethodRaw name
            FunctionRule rule -> makeMethodRaw $ BNFC.CF.funName rule
            Ident -> makeMethodRaw BNFC.CF.catIdent
        makeShiftLRaw s = text $ concat
            ["const SyntaxPrinter& operator<<(const SyntaxPrinter&, ", s, ");"]
        makeShiftL = \case
            NormalCategory name -> make name
            ListCategory {printListName = name} -> make name
            FunctionRule rule -> make $ BNFC.CF.funName rule
            Ident -> make BNFC.CF.catIdent
            where
                make s = makeShiftLRaw $ concat ["const ", s, "&"]
