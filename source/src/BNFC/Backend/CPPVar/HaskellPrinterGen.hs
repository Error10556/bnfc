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
makeHaskellPrinter opts printable listItemStorage = CPP
