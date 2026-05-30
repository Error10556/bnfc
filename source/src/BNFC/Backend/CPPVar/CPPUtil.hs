module BNFC.Backend.CPPVar.CPPUtil
    (($++$), wrapNamespace, vcatSpaced, linesToText, groupNormalizeRules
    , catNameNoCoerc, catNameWithCoerc, mergeCoercCats) where

import Prelude hiding ((<>))
import BNFC.CF
import Data.List (intercalate)
import qualified Data.Map
import Text.PrettyPrint (Doc, ($+$), text, isEmpty, empty)

-- Concats vertically with an empty line between docs
($++$) :: Doc -> Doc -> Doc
($++$) a b
    | isEmpty a = b
    | isEmpty b = a
    | otherwise = a $+$ text "" $+$ b

vcatSpaced :: [Doc] -> Doc
vcatSpaced = foldr ($++$) empty

wrapNamespace :: String -> Doc -> Doc
wrapNamespace name doc = foldr1 ($++$)
    [ text $ "namespace " ++ name ++ " {"
    , doc
    , text $ "}  // namespace " ++ name
    ]

linesToText :: [String] -> Doc
linesToText = text . intercalate "\n"

-- | normalize = turn (Cat x) into (CoercCat x 0).
--Also normalizes Rule{valRCat}.
--Returns an error (Left msg) if a category is a list with a precedence
groupNormalizeRules :: CF -> Either String (Data.Map.Map Cat [Rule])
groupNormalizeRules = foldr add (Right Data.Map.empty) . cfgRules
    where
        add :: Rule -> Either String (Data.Map.Map Cat [Rule])
            -> Either String (Data.Map.Map Cat [Rule])
        add rule =
            let rcat = valRCat rule
                normCat = case wpThing rcat of
                    Cat str -> CoercCat str 0
                    c -> c
                normRCat = rcat{wpThing = normCat}
                normRule = rule{valRCat = normRCat}
                containsCoerc = \case
                    CoercCat _ _ -> True
                    ListCat c -> containsCoerc c
                    _ -> False
                isListAndContainsCoerc = \case
                    ListCat c -> containsCoerc c
                    _ -> False
            in if isListAndContainsCoerc normCat
               then const $ Left $
                    "Lists with precedences are unsupported (category "
                    ++ catToStr normCat ++ ")"
               else fmap (Data.Map.insertWith (++) normCat [normRule])

-- | turns all KEY (CoercCat w _) into (Cat w)
mergeCoercCats :: Data.Map.Map Cat [Rule] -> Data.Map.Map Cat [Rule]
mergeCoercCats = Data.Map.fromListWith (++) . map normPair . Data.Map.toList
    where
        normPair (k, v) = (normCat k, v)

catNameNoCoerc :: Cat -> String
catNameNoCoerc = \case
    CoercCat w _ -> w
    ListCat c -> "List" ++ catNameNoCoerc c
    TokenCat w -> w
    Cat w -> w

catNameWithCoerc :: Cat -> String
catNameWithCoerc = \case
    CoercCat w n -> w ++ show n
    ListCat c -> "List" ++ catNameWithCoerc c
    TokenCat w -> w
    Cat w -> w ++ "0"
