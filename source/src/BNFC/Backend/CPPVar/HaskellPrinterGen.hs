{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.HaskellPrinterGen
  Description : Printing of the AST as a Haskell expression.

  Printing of the abstract syntax tree as a Haskell expression.
-}

module BNFC.Backend.CPPVar.HaskellPrinterGen
  (
    -- * The entrypoint
    makeHaskellPrinter

    -- * File naming
  , haskellPrinterHppFilename
  , haskellPrinterCppFilename
  ) where

-- Language imports
import Data.List (intersperse)
import Data.String.QQ (s)
import Text.PrettyPrint (($+$), Doc, nest, text)

-- BNFC imports
import qualified BNFC.CF as CF
import qualified BNFC.Options as Options

import BNFC.Backend.CPPVar.AbsynGen (tokenStorageName, ListItemStorage(..))
import BNFC.Backend.CPPVar.CPPUtil
import BNFC.Backend.CPPVar.PrinterUtils

------------------------------------------------------------------------
-- * File naming.
------------------------------------------------------------------------

-- | The name of the header file.
haskellPrinterHppFilename :: String
haskellPrinterHppFilename = "HaskellPrinter.hpp"

-- | The name of the source file.
haskellPrinterCppFilename :: String
haskellPrinterCppFilename = "HaskellPrinter.cpp"

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

makeHaskellPrinter ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> [PrintableSymbol]      -- ^ The list of types to make methods for.
  -> ListItemStorage        -- ^ How to access list items.
  -> CPPHeaderSourcePair
makeHaskellPrinter opts printable listItemStorage = CPPHeaderSourcePair
  { cppHeaderText = hpp
  , cppSourceText = cpp
  }
  where
    packwrap = wrapPackage opts

    hpp = makePrinterHeaderFile "HaskellPrinter" (unlinesToText [s|
#pragma once
#include <iostream>
#include <string_view>
#include "Absyn.hpp"
|])
      (unlinesToText [s|
    std::ostream& out;
    bool inExpression;
    friend const HaskellPrinter& operator<<(const HaskellPrinter&,
                                           std::string_view);
    HaskellPrinter(std::ostream& out, bool inExpression);
    HaskellPrinter PrintConstructorArg() const;

public:
    explicit HaskellPrinter(std::ostream& out);
|])
      printable packwrap

    cpp = text "#include \"HaskellPrinter.hpp\""
      $++$ text "#include \"PrinterCommon.hpp\""
      $++$ packwrap (printerImpl listItemStorage printable)

-- | Generates the implementation.
printerImpl ::
     ListItemStorage    -- ^ How to access list elements.
  -> [PrintableSymbol]  -- ^ All symbols to generate methods for.
  -> Doc
printerImpl listItemStorage symbols = unlinesToText [s|
HaskellPrinter::HaskellPrinter(std::ostream& out, bool inExpression)
    : out(out)
    , inExpression(inExpression) {}

HaskellPrinter::HaskellPrinter(std::ostream& out)
    : HaskellPrinter(out, false) {}

HaskellPrinter HaskellPrinter::PrintConstructorArg() const {
    return {out, true};
}
|]
  $++$ vcatSpaced (map (makeMethod listItemStorage) symbols)
  $++$ makePrinterShlImplementations "HaskellPrinter" symbols

-- | Generates an implementation of printing a class.
makeMethod ::
     ListItemStorage
  -> PrintableSymbol  -- ^ A class to print.
  -> Doc
makeMethod storeListItemsBy = \case
  PrintableNormalCategory name -> methodWrap name $ text "std::visit(*this, v);"
  PrintableList PrintableListDescription { printListName = name } ->
    let
      derefItem = case storeListItemsBy of
        StoreByValue   -> ""
        StoreByPointer -> "*"
    in
      -- For some reason, commas are not followed by spaces in list
      -- representations in system tests.
      methodWrap name $ linesToText
      [ "out << '[';"
      , "if (!v.empty()) {"
      , "    const HaskellPrinter printItem(out);"
      , "    auto last = std::prev(v.cend());"
      , "    for (auto i = v.cbegin(); i != last; ++i) {"
      , "        printItem(" ++ derefItem ++ "*i);"
      , "        out << ',';"
      , "    }"
      , "    printItem(" ++ derefItem ++ "*last);"
      , "}"
      , "out << ']';"
      ]
  PrintableFunctionRule rule -> let
      className = CF.funName rule
      body = case fieldNames $ CF.rhsRule rule of
        []     -> text ("out << \"" ++ className ++ "\";")
        fields -> linesToText
          [ "if (inExpression) out << '(';"
          , "out << \"" ++ className ++ " \";"
          , "const HaskellPrinter printField = PrintConstructorArg();"
          ]
          $+$ linesToText
            (intersperse "out << ' ';"
            [ concat
              [ "printField("
              , if isPointerCat fieldtype then "*" else ""
              , "v."
              , fieldname
              , ");"
              ]
            | (fieldname, fieldtype) <- fields])
          $+$ text "if (inExpression) out << ')';"
    in methodWrap className body
  PrintableCustomToken name -> stringlikeMethod name
  PrintableIdent   -> stringlikeMethod "Ident"
  PrintableString  -> stringlikeMethod "String"
  PrintableChar    ->
    methodWrap "Char" $ text ("PrintEscapedChar(out, v.Value);")
  PrintableDouble  -> methodWrap "Double" $ text ("PrintDouble(out, v.Value);")
  PrintableInteger -> methodWrap "Integer" $ text ("out << v.Value;")
  where
    stringlikeMethod argStructName =
      methodWrap argStructName
        $ text ("PrintEscapedString(out, v."
          ++ tokenStorageName argStructName ++ ");")
    methodWrap name body =
      text ("void HaskellPrinter::operator()(const " ++ name ++ "& v) const {")
      $+$ nest 4 body
      $+$ text "}"
    isPointerCat = \case
      CF.TokenCat _   -> False
      CF.ListCat  _   -> False
      CF.CoercCat _ _ -> True
      CF.Cat      _   -> True
