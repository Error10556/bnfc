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
import Data.String.QQ (s)
import Text.PrettyPrint (($+$), Doc, empty, nest, text)

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
makeHaskellPrinter opts printable listItemStorage = undefined
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

    cpp = text "#include \"SyntaxPrinter.hpp\""
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
  $++$ vcatSpaced (map makeMethod symbols)
  $++$ makePrinterShlImplementations "HaskellPrinter" symbols
  where
    makeMethod = undefined
