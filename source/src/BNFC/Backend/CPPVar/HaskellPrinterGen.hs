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
import Text.PrettyPrint (($+$), Doc, nest, text, empty)

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
    packwrap = nsutils_prefix $ newNamespaceUtilsFromOptions opts

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
      printable True empty packwrap

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
  PrintableNormalCategory name -> methodWrap True name
    $ text "std::visit(*this, v);"
  PrintableList PrintableListDescription { printListName = name } ->
    let
      derefItem = case storeListItemsBy of
        StoreByValue   -> ""
        StoreByPointer -> "*"
    in
      -- For some reason, commas are not followed by spaces in list
      -- representations in system tests.
      methodWrap True name $ linesToText
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
      fields = fieldNames $ CF.rhsRule rule
      body = case fields of
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
    in methodWrap (not (null fields)) className body
  PrintableCustomToken name -> stringlikeMethod name
  PrintableIdent   -> stringlikeMethod "Ident"
  PrintableString  -> stringlikeMethod "String"
  PrintableChar    ->
    methodWrap True "Char" $ text ("PrintEscapedChar(out, v.Value);")
  PrintableDouble  ->
    methodWrap True "Double" $ text ("PrintDouble(out, v.Value);")
  PrintableInteger -> methodWrap True "Integer" $ text ("out << v.Value;")
  where
    stringlikeMethod argStructName =
      methodWrap True argStructName
        $ text ("PrintEscapedString(out, v."
          ++ tokenStorageName argStructName ++ ");")
    methodWrap argUsed name body =
      text (concat
        [ "void HaskellPrinter::operator()(const "
        , name
        , "&"
        , (if argUsed then " v" else "")
        , ") const {"
        ])
      $+$ nest 4 body
      $+$ text "}"
    isPointerCat = \case
      CF.TokenCat _   -> False
      CF.ListCat  _   -> False
      CF.CoercCat _ _ -> True
      CF.Cat      _   -> True
