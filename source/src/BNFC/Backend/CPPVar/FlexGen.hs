module BNFC.Backend.CPPVar.FlexGen (flexFilename, makeFlex) where

import BNFC.CF
import BNFC.Options
import Text.PrettyPrint (Doc, text, ($+$), empty, nest)
import BNFC.Backend.CPPVar.CPPUtil
import qualified Data.Map
import qualified Data.Set
import Data.List (intercalate, sort)

flexFilename :: String -> String
flexFilename = (++".l")

makeFlex :: SharedOptions -> CF -> MkFiles ()
makeFlex opts cf =
    
