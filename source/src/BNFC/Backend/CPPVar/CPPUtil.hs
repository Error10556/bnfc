module BNFC.Backend.CPPVar.CPPUtil (($++$), wrapNamespace, linesToText) where

import Prelude hiding ((<>))
import Data.List (intercalate)
import Text.PrettyPrint (Doc, ($+$), text, isEmpty)

-- Concats vertically with an empty line between docs
($++$) :: Doc -> Doc -> Doc
($++$) a b
    | isEmpty a = b
    | isEmpty b = a
    | otherwise = a $+$ text "" $+$ b

wrapNamespace :: String -> Doc -> Doc
wrapNamespace name doc = foldr1 ($++$)
    [ text $ "namespace " ++ name ++ " {"
    , doc
    , text $ "}  // namespace " ++ name
    ]

linesToText :: [String] -> Doc
linesToText = text . intercalate "\n"
