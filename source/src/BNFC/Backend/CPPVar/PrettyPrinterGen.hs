module BNFC.Backend.CPPVar.PrettyPrinterGen
    ( prettyPrinterHppFilename
    , prettyPrinterCppFilename
    , makePrettyPrinter) where

import Text.PrettyPrint
import BNFC.Backend.CPPVar.CPPUtil
import BNFC.Backend.CPPVar.PrinterUtils
import qualified BNFC.Options

prettyPrinterHppFilename :: String
prettyPrinterHppFilename = "PrettyPrinter.hpp"

prettyPrinterCppFilename :: String
prettyPrinterCppFilename = "PrettyPrinter.cpp"

-- | -> (hpp, cpp)
makePrettyPrinter :: BNFC.Options.SharedOptions -> [PrintableSymbol]
    -> (Doc, Doc)
makePrettyPrinter = 
