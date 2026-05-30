{-
    BNF Converter -> C++17 with std::variant
    (C) (2026) Author: Timur Usmanov <t.usmanov@innopolis.university>
-}

{-# LANGUAGE MultilineStrings #-}
module BNFC.Backend.CPPVar (makeCppVar) where

--import BNFC.Utils
import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import qualified BNFC.Backend.CPPVar.CPPUtil as CPPUtil
import BNFC.Backend.CPPVar.AbsynGen
import qualified BNFC.Backend.C as BackendC (comment)

makeCppVar :: SharedOptions -> CF -> MkFiles ()
makeCppVar opts cf = do
    let groupedRules = case CPPUtil.groupNormalizeRules cf of
            Left msg -> error msg
            Right val -> val
        (absynHpp, absynCpp) = makeAbsyn opts cf groupedRules
    mkfile absynHppFilename BackendC.comment absynHpp
    mkfile absynCppFilename BackendC.comment absynCpp
