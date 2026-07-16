{-|
  Module      : BNFC.Backend.CPPVar.CPPUtil
  Description : Common functionality.

  Common functionality.
-}

module BNFC.Backend.CPPVar.CPPUtil
  (
    -- * C++ files
    CPPHeaderSourcePair(..)

    -- * Additional functions on 'Text.PrettyPrint.Doc'
  , ($++$)
  , linesToText
  , unlinesToText
  , wrapNamespace
  , wrapPackage
  , vcatSpaced

    -- * Types & functions to use with grammar rules
  , NontokenCategory(..)
  , NontokenClassCategory(..)
  , GroupedRules(..)
  , MergedGroupedRules(..)
  , nontoken2cat
  , groupRules
  , fieldNames
  , mergeCoercCats
  , isClassLabel

    -- * Category functions
  , removePrecedenceFromCat
  , removePrecedenceFromCats
  , extractEntrypoints
  , catNameNoCoerc
  , catNameWithCoerc
  , nontokenCatNameNoCoerc
  , nontokenCatNameWithCoerc
  , nontokenClassCatName

    -- * UTF-8
  , utf8encode
  , cppShowString
  ) where

-- Language imports
import Prelude hiding ((<>))
import Data.List (sort)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, chr, ord)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Bits
import Data.Int (Int8)

import Text.PrettyPrint (Doc, ($+$), text, isEmpty, empty)

-- BNFC imports
import qualified BNFC.Options as Options
import qualified BNFC.CF as CF
import Numeric (showOct)

------------------------------------------------------------------------
-- * C++ files.
------------------------------------------------------------------------

-- | A record returned from some code-generator functions,
-- contains the text to put in the header and the source files.
data CPPHeaderSourcePair = CPPHeaderSourcePair
  { cppHeaderText :: !Doc
    -- ^ The content of the header file.
  , cppSourceText :: !Doc
    -- ^ The content of the source file.
  }

------------------------------------------------------------------------
-- * Additional functions on 'Text.PrettyPrint.Doc'.
------------------------------------------------------------------------

-- | Concatenates vertically with an empty line between the blocks.
($++$) ::
     Doc  -- ^ The upper block.
  -> Doc  -- ^ The lower block.
  -> Doc  -- ^ The combination.
($++$) a b
  | isEmpty a = b
  | isEmpty b = a
  | otherwise = a $+$ text "" $+$ b

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

-- | Concatenates multiple blocks vertically inserting an empty line inbetween
-- each two.
vcatSpaced :: [Doc] -> Doc
vcatSpaced = foldr ($++$) empty

------------------------------------------------------------------------
-- * Types & functions to use with grammar rules.
------------------------------------------------------------------------

-- | A grammar category that is not a v'BNFC.CF.TokenCat'.
data NontokenCategory
  = Nontoken_Cat      !String           -- ^ As v'BNFC.CF.Cat'.
  | Nontoken_CoercCat !String !Integer  -- ^ As v'BNFC.CF.CoercCat'.
  | Nontoken_ListCat  !CF.Cat           -- ^ As v'BNFC.CF.ListCat'.
  deriving (Eq, Ord, Show)

-- | A grammar category that is neither a v'BNFC.CF.TokenCat'
-- nor a v'BNFC.CF.CoercCat'.
data NontokenClassCategory
  = NontokenClass_Cat     !String  -- ^ As v'BNFC.CF.Cat'.
  | NontokenClass_ListCat !CF.Cat  -- ^ As v'BNFC.CF.ListCat'.
  deriving (Eq, Ord, Show)

-- | Rules grouped by the category.
newtype GroupedRules = GroupedRules (Map NontokenCategory [CF.Rule])

-- | Rules
newtype MergedGroupedRules
  = MergedGroupedRules (Map NontokenClassCategory [CF.Rule])

-- | Generalizes a t'NontokenCategory'.
nontoken2cat :: NontokenCategory -> CF.Cat
nontoken2cat = \case
  Nontoken_Cat name         -> CF.Cat name
  Nontoken_CoercCat name lv -> CF.CoercCat name lv
  Nontoken_ListCat elemCat  -> CF.ListCat elemCat

-- | Group rules by the category.
groupRules ::
     [CF.Rule]  -- ^ The grammar rules.
  -> GroupedRules
groupRules = GroupedRules . foldr add Map.empty
  where
    add ::
         CF.Rule
      -> Map NontokenCategory [CF.Rule]
      -> Map NontokenCategory [CF.Rule]
    add rule = let
        cat = case CF.wpThing (CF.valRCat rule) of
          CF.Cat name           -> Nontoken_Cat name
          CF.CoercCat name prec -> Nontoken_CoercCat name prec
          CF.ListCat elemCat    -> Nontoken_ListCat elemCat
          CF.TokenCat _         ->
            error "Grammar has a production rule for a token"
      in
        Map.insertWith (++) cat [rule]

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

-- | Removes precedence information from the categories (keys) of
-- t'GroupedRules'. Does not change the 'BNFC.CF.Rule's (values).
--
-- See 'removePrecedenceFromCat'.
mergeCoercCats :: GroupedRules -> MergedGroupedRules
mergeCoercCats (GroupedRules rulemap) =
  MergedGroupedRules $ Map.fromListWith (++) $ map normPair $ Map.toList rulemap
  where
    -- | Does NOT normalize away list items, e.g. [Expr1] -/-> [Expr]
    normPair (k, v) = (normKey k, v)
    normKey = \case
      Nontoken_Cat name        -> NontokenClass_Cat name
      Nontoken_CoercCat name _ -> NontokenClass_Cat name
      Nontoken_ListCat elemCat -> NontokenClass_ListCat elemCat

-- Checks if this label corresponds to a C++ class (as opposed to user-defined
-- functions or special labels like "_").
isClassLabel :: String -> Bool
isClassLabel = \case
  ""        -> False
  first : _ -> isAsciiUpper first

------------------------------------------------------------------------
-- * Category functions.
------------------------------------------------------------------------

-- | The correct precedence removal function.
-- Preserves precedence in list elements.
removePrecedenceFromCat :: CF.Cat -> CF.Cat
removePrecedenceFromCat = \case
  CF.CoercCat s _ -> CF.Cat s
  other           -> other

-- | Removes precedence information and deduplicates the categories.
removePrecedenceFromCats :: [CF.Cat] -> [CF.Cat]
removePrecedenceFromCats = Set.toList . Set.fromList
  . map removePrecedenceFromCat

-- | Returns a list of categories to use as parse targets.
-- If the grammar does not specify them explicitly, returns all categories that
-- __cannot be converted__ to others without additional syntax.
-- __Keeps__ precedence information.
-- Deduplicates specified categories.
extractEntrypoints ::
     [CF.Pragma]   -- ^ Grammar pragmas (contain @entrypoint@ declarations).
  -> GroupedRules  -- ^ Rules grouped by category.
  -> [CF.Cat]
extractEntrypoints pragmas (GroupedRules rulemap)
  | null res  =
    -- Set.toList $ Set.fromList
    -- $ map (removePrecedenceFromCat . nontoken2cat) $ Map.keys rulemap
    let
      allCats  = map nontoken2cat $ Map.keys rulemap
      allRules = concat $ Map.elems rulemap
      badCats  =
        [ onlycat
        | CF.Rule { rhsRule = [Left onlycat], internal = CF.Parsable }
          <- allRules
        ]
    in Set.toList $ Set.fromList allCats `Set.difference` Set.fromList badCats
  | otherwise = res
  where
    res = Set.toList $ Set.fromList $ map CF.wpThing
      $ concat [cats | CF.EntryPoints cats <- pragmas]

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

-- | For a given nonterminal, returns a C identifier suitable for a class name
-- (drops precedence information).
nontokenCatNameNoCoerc :: NontokenCategory -> String
nontokenCatNameNoCoerc = catNameNoCoerc . nontoken2cat

-- | For a given nonterminal, returns a C identifier preserving precedence
-- information.
nontokenCatNameWithCoerc :: NontokenCategory -> String
nontokenCatNameWithCoerc = catNameWithCoerc . nontoken2cat

-- | For a given nonterminal, returns a C identifier suitable for a class name.
nontokenClassCatName :: NontokenClassCategory -> String
nontokenClassCatName = \case
  NontokenClass_Cat     n       -> normalizeCPPName n
  NontokenClass_ListCat elemCat -> "List" ++ catNameWithCoerc elemCat

------------------------------------------------------------------------
-- * UTF-8.
------------------------------------------------------------------------

-- | Encodes a (unicode) character into a list of bytes in the UTF-8 encoding.
--
-- See @man 7 utf-8@.
utf8encode :: Int -> [Int8]
utf8encode = map fromIntegral . helper
  where
    helper c
      | c < 0          = error "Negative char"
      | c <= 0x7f      = [c]
      | c <= 0x7ff     = [0xc0 + shiftR6 1 c, 0x80 + (c .&. 0x3F)]
      | c <= 0xffff    =
        [0xe0 + shiftR6 2 c, 0x80 + shiftR6 1 c, 0x80 + (c .&. 0x3f)]
      | c <= 0x1fffff  =
        [ 0xf0 + shiftR6 3 c
        , 0x80 + shiftR6 2 c
        , 0x80 + shiftR6 1 c
        , 0x80 + (c .&. 0x3f)
        ]
      | c <= 0x3ffffff =
        [ 0xf8 + shiftR6 4 c
        , 0x80 + shiftR6 3 c
        , 0x80 + shiftR6 2 c
        , 0x80 + shiftR6 1 c
        , 0x80 + (c .&. 0x3f)
        ]
      | otherwise      =
        [ 0xfc + shiftR6 5 c
        , 0x80 + shiftR6 4 c
        , 0x80 + shiftR6 3 c
        , 0x80 + shiftR6 2 c
        , 0x80 + shiftR6 1 c
        , 0x80 + (c .&. 0x3f)
        ]
      where
        shiftR6 n c = (c `shiftR` (6 * n)) .&. 0x3f

-- | A string literal in C++ with the given value (encoded in UTF-8).
cppShowString ::
     String  -- ^ The value.
  -> String  -- ^ The representation in C++ in UTF-8.
cppShowString s =
  '"' : (concatMap reprChar (concatMap (utf8encode . ord) s) ++ "\"")
  where
    reprChar :: Int8 -> String
    reprChar = \case
      7  -> "\\a"
      8  -> "\\b"
      9  -> "\\t"
      10 -> "\\n"
      11 -> "\\v"
      12 -> "\\f"
      13 -> "\\r"
      34 -> "\\\""
      92 -> "\\\\"
      other ->
        if 32 <= other && other < 127
        then [chr $ fromIntegral other]
        else
          let
            code =
              if other < 0
              then 256 + fromIntegral other :: Int
              else fromIntegral other :: Int
          in '\\' : pad3 (showOct code "")
    pad3 s = replicate (3 - length s) '0' ++ s

------------------------------------------------------------------------
-- * Utility.
------------------------------------------------------------------------

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
