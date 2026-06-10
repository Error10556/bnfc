{-# LANGUAGE QuasiQuotes #-}

module BNFC.Backend.CPPVar.PrettyPrinterGen
    ( prettyPrinterHppFilename
    , prettyPrinterCppFilename
    , makePrettyPrinter) where

import Text.PrettyPrint
import BNFC.Backend.CPPVar.CPPUtil
import BNFC.Backend.CPPVar.PrinterUtils
import Data.String.QQ
import qualified BNFC.Options

prettyPrinterHppFilename :: String
prettyPrinterHppFilename = "PrettyPrinter.hpp"

prettyPrinterCppFilename :: String
prettyPrinterCppFilename = "PrettyPrinter.cpp"

unlinesToText :: String -> Doc
unlinesToText = linesToText . lines

-- | -> (hpp, cpp)
makePrettyPrinter :: BNFC.Options.SharedOptions -> [PrintableSymbol]
    -> (Doc, Doc)
makePrettyPrinter opts printable = (hpp, cpp)
    where
        hpp = unlinesToText [s|
// The default pretty printer does not suit all languages.
// See PrettyPrinter.cpp for details.

#pragma once
#include <iostream>
#include <string_view>
#include "Absyn.hpp"
|] $++$ packwrap (printerClassDecl printable)

        cpp = empty
        packwrap = wrapPackage opts

printerClassDecl :: [PrintableSymbol] -> Doc
printerClassDecl printable = unlinesToText [s|
class PrettyPrinter {
    std::ostream& out;
    unsigned int indent;
    int coercionLevel;
    void NewLine(unsigned int indent) const;

    friend const PrettyPrinter& operator<<(const PrettyPrinter&,
                                           std::string_view);

public:
    PrettyPrinter(std::ostream&, unsigned int indent = 0,
                  int coercionLevel = 0);
    PrettyPrinter Indented(unsigned int plusIndent = 4,
                           int coercionLevel = 0) const;
    PrettyPrinter Dedented(unsigned int minusIndent = 4,
                           int coercionLevel = 0) const;
    PrettyPrinter WithCoercionLevel(int level) const;
    void NewLine() const;
|] $+$ nest 4 (linesToText (map method printable)) $+$ text "};"
    $++$ linesToText (map operatorShL printable) $+$ text [s|
const PrettyPrinter& operator<<(const PrettyPrinter&, std::string_view);
|]
    where
        method p =
            "void operator()(const " ++ printableClassName p ++ "&) const;"
        operatorShL p =
            "const PrettyPrinter& operator<<(const PrettyPrinter&, const "
            ++ printableClassName p ++ "&);"
