module BNFC.Backend.CPPVar.CPPUtil
  (($++$)
  , wrapNamespace
  , wrapPackage
  , vcatSpaced
  , linesToText
  , unlinesToText
  , groupRules
  , catNameNoCoerc
  , catNameWithCoerc
  , mergeCoercCats
  , GroupedRules
  , fieldNames) where

import Prelude hiding ((<>))
import BNFC.CF
import qualified BNFC.Options
import qualified Data.Map
import Text.PrettyPrint (Doc, ($+$), text, isEmpty, empty)
import Data.Char
import qualified Data.Set
import Data.List (sort)

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

unlinesToText :: String -> Doc
unlinesToText = linesToText . lines

type GroupedRules = Data.Map.Map Cat [Rule]

groupRules :: CF -> GroupedRules
groupRules = foldr add Data.Map.empty . cfgRules
  where
    add :: Rule -> Data.Map.Map Cat [Rule] -> Data.Map.Map Cat [Rule]
    add rule = let
        rcat = valRCat rule
        cat = wpThing rcat
      in Data.Map.insertWith (++) cat [rule]

-- | turns all KEY (CoercCat w _) into (Cat w)
mergeCoercCats :: Data.Map.Map Cat [Rule] -> Data.Map.Map Cat [Rule]
mergeCoercCats = Data.Map.fromListWith (++) . map normPair . Data.Map.toList
  where
    normPair (k, v) = (normCatNoList k, v)
    -- | Does NOT normalize away list items, e.g. [Expr1] -/-> [Expr]
    normCatNoList = \case
      CoercCat s _ -> Cat s
      other -> other

normalizeCPPName :: String -> String
normalizeCPPName =
  (\case [] -> "_"; s@(ch:_) -> if isAlpha_ ch then s else '_' : s)
  . map (\ch -> if isAlnum ch then ch else '_')
  where
    isAlpha_ ch = isAsciiLower ch || isAsciiUpper ch || ch == '_'
    isAlnum ch = isAlpha_ ch || isDigit ch

catNameNoCoerc :: Cat -> String
catNameNoCoerc = \case
  CoercCat w _ -> normalizeCPPName w
  -- Lists of different coercions are different, so distinguish by name
  ListCat c -> "List" ++ catNameWithCoerc c
  TokenCat w -> w
  Cat w -> normalizeCPPName w

catNameWithCoerc :: Cat -> String
catNameWithCoerc = \case
  CoercCat w n -> normalizeCPPName w ++ show n
  ListCat c -> "List" ++ catNameWithCoerc c
  TokenCat w -> w
  Cat w -> normalizeCPPName w

fieldNames :: SentForm -> [(String, Cat)]
fieldNames sentForm = let
    members = [cat | (Left cat) <- sentForm]
    unindexedNames = map ((++"_") . catNameNoCoerc) members
    indexedNames = indexNames' unindexedNames
  in
    zip indexedNames members
  where
    indexNames' names =
      help names Data.Map.empty
      where
        nonuniq = Data.Set.fromList $ nonunique names
        help :: [String] -> Data.Map.Map String Int -> [String]
        help [] _ = []
        help (name:tail) prevs = if name `elem` nonuniq
          then
            let curindex = maybe 1 (+1) (Data.Map.lookup name prevs)
            in (name ++ show curindex) :
              help tail (Data.Map.insert name curindex prevs)
          else name : help tail prevs

nonunique :: (Ord a, Eq a) => [a] -> [a]
nonunique lst = case sort lst of
  [] -> []
  a:tail -> help True a tail
  where
    help enabled prev tail = case tail of
      [] -> []
      x:tail' -> if prev == x
        then (if enabled
          then x : help False x tail'
          else help False x tail')
        else help True x tail'
