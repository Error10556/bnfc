module BNFC.Backend.CPPVar.CPPUtil
    (($++$)
    , wrapNamespace
    , wrapPackage
    , vcatSpaced
    , linesToText
    , groupRules
    , catNameNoCoerc
    , catNameWithCoerc
    , mergeCoercCats
    , GroupedRules) where

import Prelude hiding ((<>))
import BNFC.CF
import qualified BNFC.Options
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

wrapPackage :: BNFC.Options.SharedOptions -> Doc -> Doc
wrapPackage opts = maybe id wrapNamespace (BNFC.Options.inPackage opts)

linesToText :: [String] -> Doc
linesToText = foldr ($+$) empty . map text

type GroupedRules = Data.Map.Map Cat [Rule]

-- | Returns an error (Left msg) if a category is a list with a precedence
groupRules :: CF -> Either String GroupedRules
groupRules = foldr add (Right Data.Map.empty) . cfgRules
    where
        add :: Rule -> Either String (Data.Map.Map Cat [Rule])
            -> Either String (Data.Map.Map Cat [Rule])
        add rule =
            let rcat = valRCat rule
                cat = wpThing rcat
                containsCoerc = \case
                    CoercCat _ _ -> True
                    ListCat c -> containsCoerc c
                    _ -> False
                isListAndContainsCoerc = \case
                    ListCat c -> containsCoerc c
                    _ -> False
            in if isListAndContainsCoerc cat
               then const $ Left $
                    "Lists with precedences are unsupported (category "
                    ++ catToStr cat ++ ")"
               else fmap (Data.Map.insertWith (++) cat [rule])

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
    ListCat c -> "List" ++ catNameNoCoerc c  -- NoCoerc for lists
    TokenCat w -> w
    Cat w -> w ++ "0"
