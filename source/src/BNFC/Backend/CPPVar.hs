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
import BNFC.Backend.CPPVar.AbsynGen
import qualified BNFC.Backend.C as BackendC (comment)

makeCppVar :: SharedOptions -> CF -> MkFiles ()
makeCppVar opts cf = do
    let (absynHpp, absynCpp) = makeAbsyn opts cf
    mkfile absynHppFilename BackendC.comment absynHpp
    mkfile absynCppFilename BackendC.comment absynCpp
