{-|
  Module      : BNFC.Backend.CPPVar.CPPUtil
  Description : Common functionality.

  Common functionality.
-}

module BNFC.Backend.CPPVar.CPPUtil
  (
    -- * C++ files
    CPPHeaderSourcePair(..)

    -- * additional functions on 'Text.PrettyPrint.Doc'
  , ($++$)
  , linesToText
  , unlinesToText
  , wrapNamespace
  , wrapPackage
  , vcatSpaced

    -- * Functions to use with grammar rules
  , GroupedRules
  , groupRules
  , fieldNames
  , mergeCoercCats

    -- * Category functions
  , removePrecedenceFromCat
  , catNameNoCoerc
  , catNameWithCoerc
  ) where

-- Language imports
import Prelude hiding ((<>))
import Data.List (sort)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

import Text.PrettyPrint (Doc, ($+$), text, isEmpty, empty)

-- BNFC imports
import qualified BNFC.Options as Options
import qualified BNFC.CF as CF
import BNFC.CF (CF)

-- | A record returned from some code-generator functions,
-- contains the text to put in the header and the source files.
data CPPHeaderSourcePair = CPPHeaderSourcePair
  { cppHeaderText :: !Doc
    -- ^ The content of the header file.
  , cppSourceText :: !Doc
    -- ^ The content of the source file.
  }


-- | Concatenates vertically with an empty line between the blocks.
($++$) ::
     Doc  -- ^ The upper block.
  -> Doc  -- ^ The lower block.
  -> Doc  -- ^ The combination.
($++$) a b
  | isEmpty a = b
  | isEmpty b = a
  | otherwise = a $+$ text "" $+$ b

-- | Concatenates multiple blocks vertically inserting an empty line inbetween
-- each two.
vcatSpaced :: [Doc] -> Doc
vcatSpaced = foldr ($++$) empty

-- | Wraps a code block into a namespace. Does not indent the block.
wrapNamespace ::
     String  -- ^ Namespace name.
  -> Doc     -- ^ Code to wrap.
  -> Doc
wrapNamespace name doc = foldr1 ($++$)
  [ text $ "namespace " ++ name ++ " {"
  , doc
  , text $ "}  // namespace " ++ name
  ]

-- | Wraps a code block into a namespace given by 'BNFC.Options.inPackage'.
-- If no package has been specified, does nothing.
wrapPackage ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> Doc                    -- ^ Code to maybe wrap.
  -> Doc
wrapPackage opts = maybe id wrapNamespace (Options.inPackage opts)

-- | Converts a list of individual lines into a block of text.
--
-- Is /not/ equivalent to @text . unlines@ because the latter returns a block
-- that thinks it contains exactly one line.
linesToText :: [String] -> Doc
linesToText = foldr ($+$) empty . map text

-- | Converts a (possibly multiline) string into a block of text.
--
-- In particular, @unlinesToText ""@ returns 'Text.PrettyPrint.empty'.
--
-- Is /not/ equivalent to @text@ because the latter returns a block
-- that thinks it contains exactly one line.
unlinesToText :: String -> Doc
unlinesToText = linesToText . lines

-- | Rules grouped by the category.
type GroupedRules = Map CF.Cat [CF.Rule]

-- | Group rules by the category.
groupRules ::
     CF  -- ^ The grammar description.
  -> GroupedRules
groupRules = foldr add Map.empty . CF.cfgRules
  where
    add :: CF.Rule -> GroupedRules -> GroupedRules
    add rule =
      let
        rcat = CF.valRCat rule
        cat  = CF.wpThing rcat
      in Map.insertWith (++) cat [rule]

-- | Removes precedence information from the categories (keys) of
-- 'GroupedRules'. Does not change the 'BNFC.CF.Rule's (values).
--
-- See 'removePrecedenceFromCat'.
mergeCoercCats :: GroupedRules -> GroupedRules
mergeCoercCats = Map.fromListWith (++) . map normPair . Map.toList
  where
    -- | Does NOT normalize away list items, e.g. [Expr1] -/-> [Expr]
    normPair (k, v) = (removePrecedenceFromCat k, v)

-- | The correct precedence removal function.
-- Preserves precedence in list elements.
removePrecedenceFromCat :: CF.Cat -> CF.Cat
removePrecedenceFromCat = \case
  CF.CoercCat s _ -> CF.Cat s
  other           -> other

-- | Transforms a string into a valid C identifier.
--
--  * Replaces invalid characters with @_@;
--  * Ensures that the identifier starts with a letter or @_@.
normalizeCPPName :: String -> String
normalizeCPPName =
  (\case
    []       -> "_"
    s@(ch:_) ->
      if isAlpha_ ch
      then s
      else '_' : s
  ) . map (\ ch ->
    if isAlnum ch
    then ch
    else '_')
  where
    isAlpha_ ch = isAsciiLower ch || isAsciiUpper ch || ch == '_'
    isAlnum ch  = isAlpha_ ch || isDigit ch

-- | For a given nonterminal, returns a C identifier suitable for a class name
-- (drops precedence information).
catNameNoCoerc :: CF.Cat -> String
catNameNoCoerc = \case
  CF.CoercCat w _ -> normalizeCPPName w
  -- Lists of different coercions are different, so distinguish by name
  CF.ListCat c -> "List" ++ catNameWithCoerc c
  CF.TokenCat w -> w
  CF.Cat w -> normalizeCPPName w

-- | For a given nonterminal, returns a C identifier preserving precedence
-- information.
catNameWithCoerc :: CF.Cat -> String
catNameWithCoerc = \case
  CF.CoercCat w n -> normalizeCPPName w ++ show n
  CF.ListCat c -> "List" ++ catNameWithCoerc c
  CF.TokenCat w -> w
  CF.Cat w -> normalizeCPPName w

-- | Returns appropriate class field names for a class representing a BNFC
-- label. Numbers similar names.
fieldNames :: CF.SentForm -> [(String, CF.Cat)]
fieldNames sentForm = let
    members = [cat | (Left cat) <- sentForm]
    unindexedNames = map ((++ "_") . catNameNoCoerc) members
    indexedNames = indexNames' unindexedNames
  in
    zip indexedNames members
  where
    indexNames' names =
      help names Map.empty
      where
        nonuniq = Set.fromList $ nonunique names
        help :: [String] -> Data.Map.Map String Int -> [String]
        help [] _ = []
        help (name : tail) prevs =
          if name `elem` nonuniq
          then
            let curindex = maybe 1 (+1) (Map.lookup name prevs)
            in (name ++ show curindex) :
              help tail (Map.insert name curindex prevs)
          else name : help tail prevs

-- | Returns a (deduplicated) list of elements that occur more than once in the
-- given list. \( O(n \log n) \).
nonunique :: (Ord a, Eq a) => [a] -> [a]
nonunique lst = case sort lst of
    [] -> []
    a : tail -> help True a tail
  where
    help enabled prev tail = case tail of
      []        -> []
      x : tail' ->
        if prev == x
        then
          (if enabled
          then x : help False x tail'
          else help False x tail')
        else help True x tail'
