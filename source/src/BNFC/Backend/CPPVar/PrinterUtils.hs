{-|
  Module      : BNFC.Backend.CPPVar.PrinterUtils
  Description : Extracts printable symbols from grammar descriptions.
-}

module BNFC.Backend.CPPVar.PrinterUtils
  (
    -- * The printable symbol
    PrintableSymbol(..)
  , PrintableListDescription(..)
  , getPrintableSymbols

    -- * Conversions
  , literalName2Symbol
  , printableClassName
  ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Map (Map)

import qualified BNFC.CF as CF
import BNFC.CF (CF)
import BNFC.Backend.CPPVar.CPPUtil

-- | A C++ type that must be supported by the printer.
data PrintableSymbol
  = PrintableNormalCategory !String  -- ^ Represents a @std::variant@ (a BNFC category).
  | PrintableList !PrintableListDescription
    -- ^ Represents a collection (currently derived from @std::deque@)
    -- generated from a BNFC list.
  | PrintableFunctionRule !CF.Rule
    -- ^ Represents a class generated from a normal rule.
  | PrintableCustomToken !String
    -- ^ Represents a token struct generated from a user-defined token.
  | PrintableIdent    -- ^ Represents an @Ident@ token.
  | PrintableString   -- ^ Represents a @String@ token.
  | PrintableChar     -- ^ Represents a @Char@ token.
  | PrintableDouble   -- ^ Represents a @Double@ token.
  | PrintableInteger  -- ^ Represents an @Integer@ token.

-- | Information needed to completely pretty-print a list.
data PrintableListDescription = PrintableListDescription
  { printListName      :: !String
    -- ^ The class name in C++.
  , printListItemCoerc :: !Integer
    -- ^ The expected precedence of the items.
  , printListEmpty     :: !(Maybe [String])
    -- ^ Representation of an empty list, @[]@.
  , printListCons      ::
      !(Maybe ([String], CF.Cat, [String], CF.Cat, [String]))
    -- ^ Representation of a @cons@ operation, @(:)@.
  , printListSingle    :: !(Maybe ([String], CF.Cat, [String]))
    -- ^ Representation of a singleton list, @(:[])@.
  }

-- | Extracts printable symbols from the grammar description.
getPrintableSymbols ::
     CF            -- ^ The grammar description.
  -> GroupedRules  -- ^ Grouped rules from the same grammar
                   -- (provided to not recompute them).
  -> [PrintableSymbol]
getPrintableSymbols cf rulemap = literals ++ customTokens ++ nonliterals
  where
    literals = map (\ name -> case name `Map.lookup` literalName2Symbol of
        Just printable -> printable
        Nothing        -> error $ "Unsupported literal: " ++ name
      ) $ CF.cfgLiterals cf
    customTokens =
      [ PrintableCustomToken $ CF.wpThing name
      | CF.TokenReg name _ _ <- CF.cfgPragmas cf]
    nonliterals = concat
      $ flip map (Map.toList (mergeCoercCats rulemap)) $ \case
        (cat@(CF.ListCat itemcat), rules) -> [PrintableList
          $ PrintableListDescription
            { printListName = catNameNoCoerc cat
            , printListItemCoerc = case itemcat of CF.CoercCat _ i -> i; _ -> 0
            , printListEmpty = parseEmptyList <$> Map.lookup "[]" mapRules
            , printListCons = parseCons <$> Map.lookup "(:)" mapRules
            , printListSingle = parseSingleton <$> Map.lookup "(:[])" mapRules
            }]
          where
            mapRules = Map.fromList [(CF.funName r, CF.rhsRule r) | r <- rules]
        (cat@(CF.Cat _),        rules) ->
          makeNormalCategory (catNameNoCoerc cat) rules
        (cat@(CF.CoercCat _ _), rules) ->
          makeNormalCategory (catNameNoCoerc cat) rules
        (CF.TokenCat _,         _)     -> error "TokenCat in GroupedRules"

    makeNormalCategory :: String -> [CF.Rule] -> [PrintableSymbol]
    makeNormalCategory catname rules = PrintableNormalCategory catname :
      map PrintableFunctionRule (filter (\ r -> CF.funName r /= "_") rules)

    -- | Consumes all strings (@Right@ values) from the start of the
    -- t'CF.SentForm', returning them in a list.
    -- Also returns the rest of the t'CF.SentForm'.
    readStrings :: CF.SentForm -> ([String], CF.SentForm)
    readStrings sentForm = case sentForm of
      []             -> ([], sentForm)
      Left _  : _    -> ([], sentForm)
      Right s : tail ->
        let (ss, resform) = readStrings tail
        in (s : ss, resform)

    -- | Returns \(n\) t'Cat's and \(n+1\) @[String]@s, reading interleaved.
    parseSentForm :: Int -> CF.SentForm -> ([CF.Cat], [[String]])
    parseSentForm n sentForm
      | n == 0    = case tail of
        [] -> ([], [strings])
        _ -> error "Too many categories in a sentence form"
      | otherwise = case tail of
        Left cat : tail2 ->
          let (cats', strs') = parseSentForm (n - 1) tail2
          in (cat : cats', strings : strs')
        _                -> error "Too few categories in a sentence form"
      where
        (strings, tail) = readStrings sentForm

    parseEmptyList :: CF.SentForm -> [String]
    parseEmptyList sentForm =
      let ([], [ss]) = parseSentForm 0 sentForm in ss
    parseSingleton :: CF.SentForm -> ([String], CF.Cat, [String])
    parseSingleton sentForm =
      let ([c], [ss1, ss2]) = parseSentForm 1 sentForm
      in (ss1, c, ss2)
    parseCons :: CF.SentForm -> ([String], CF.Cat, [String], CF.Cat, [String])
    parseCons sentForm =
      let ([c1, c2], [ss1, ss2, ss3]) = parseSentForm 2 sentForm
      in (ss1, c1, ss2, c2, ss3)

-- | Maps built-in token names to t'PrintableSymbol's.
literalName2Symbol :: Map CF.Literal PrintableSymbol
literalName2Symbol = Map.fromList
  [ (CF.catIdent,   PrintableIdent)
  , (CF.catString,  PrintableString)
  , (CF.catInteger, PrintableInteger)
  , (CF.catDouble,  PrintableDouble)
  , (CF.catChar,    PrintableChar)
  ]

-- | Returns the C++ type name of the t'PrintableSymbol'.
printableClassName :: PrintableSymbol -> String
printableClassName = \case
  PrintableList (PrintableListDescription {printListName = s}) -> s
  PrintableNormalCategory s -> s
  PrintableFunctionRule r   -> CF.funName r
  PrintableCustomToken t    -> t
  PrintableIdent            -> CF.catIdent
  PrintableChar             -> CF.catChar
  PrintableString           -> CF.catString
  PrintableDouble           -> CF.catDouble
  PrintableInteger          -> CF.catInteger
