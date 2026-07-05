{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.PrettyPrinterGen
  Description : Heuristic-driven pretty-printing of the abstract syntax tree.

  Heuristic-driven pretty-printing of the abstract syntax tree.
-}

module BNFC.Backend.CPPVar.PrettyPrinterGen
  (
    -- * The entrypoint
    makePrettyPrinter

    -- * File naming
  , prettyPrinterHppFilename
  , prettyPrinterCppFilename
  ) where

-- Language imports
import Data.String.QQ

import Text.PrettyPrint hiding (Str)

-- BNFC imports
import qualified BNFC.Options
import qualified BNFC.CF as CF

import BNFC.Backend.CPPVar.CPPUtil
import BNFC.Backend.CPPVar.PrinterUtils
import BNFC.Backend.CPPVar.AbsynGen (tokenStorageName, ListItemStorage(..))

------------------------------------------------------------------------
-- * File naming.
------------------------------------------------------------------------

-- | The name of the header file.
prettyPrinterHppFilename :: String
prettyPrinterHppFilename = "PrettyPrinter.hpp"

-- | The name of the source file.
prettyPrinterCppFilename :: String
prettyPrinterCppFilename = "PrettyPrinter.cpp"

------------------------------------------------------------------------
-- * The entrypoint.
------------------------------------------------------------------------

-- | Generates the @PrettyPrinter@ class (declaration and implementation).
makePrettyPrinter ::
     BNFC.Options.SharedOptions  -- ^ BNFC invokation options.
  -> [PrintableSymbol]           -- ^ The list of types to make methods for.
  -> ListItemStorage             -- ^ How to access list elements.
  -> CPPHeaderSourcePair
makePrettyPrinter opts printable listItemStorage = CPPHeaderSourcePair
  { cppHeaderText = hpp
  , cppSourceText = cpp
  }
  where
    hpp = unlinesToText [s|
// The default pretty printer does not suit all languages.
// See PrettyPrinter.cpp for details.

#pragma once
#include <iostream>
#include <string_view>
#include "Absyn.hpp"
|] $++$ packwrap (printerClassDecl printable)

    cpp = disclaimer $++$ unlinesToText [s|
#include "PrettyPrinter.hpp"

#include "PrinterCommon.hpp"
|] $++$ packwrap
      (printerUtilImpl
      $++$ vcatSpaced (map (makeMethod listItemStorage) printable)
      $++$ operatorShLImpl printable)

    packwrap = wrapPackage opts

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

-- | Generates the class declaration with t'PrintableSymbol's translated to
-- methods.
printerClassDecl ::
     [PrintableSymbol]  -- ^ What to support printing.
  -> Doc
printerClassDecl printable = unlinesToText [s|
class PrettyPrinter {
    std::ostream& out;
    unsigned int indent;
    int coercionLevel;
    void NewLine(unsigned int indent) const;

    friend const PrettyPrinter& operator<<(const PrettyPrinter&,
                                           std::string_view);

public:
    PrettyPrinter(std::ostream&, unsigned int indent = 0,
                  int coercionLevel = 0);
    PrettyPrinter Indented(unsigned int plusIndent = 4,
                           int coercionLevel = 0) const;
    PrettyPrinter Dedented(unsigned int minusIndent = 4,
                           int coercionLevel = 0) const;
    PrettyPrinter WithCoercionLevel(int level) const;
    void NewLine() const;
|] $+$ nest 4 (linesToText (map method printable)) $+$ text "};"
  $++$ linesToText (map operatorShL printable) $+$ text [s|
const PrettyPrinter& operator<<(const PrettyPrinter&, std::string_view);
|]
  where
    method p =
      "void operator()(const " ++ printableClassName p ++ "&) const;"
    operatorShL p =
      "const PrettyPrinter& operator<<(const PrettyPrinter&, const "
      ++ printableClassName p ++ "&);"

-- | A notice to the user about the limitations.
disclaimer :: Doc
disclaimer = unlinesToText [s|
/**** Disclaimer ****
 * The default PrettyPrinter implementation makes a number of assumptions about
 * the target language. Namely:
 *
 * * All tokens are separated from each other by a space, except for commas ','
 *   and semicolons ';', which are only separated from the right, and brackets
 *  '[]' or parentheses '()', which are not separated from the enclosed text.
 *
 * * Curly braces '{}' (and only those) enclose an indented block. The
 *   indentation equals 4 spaces. The left curly brace causes one line break
 *   _after_ itself, the right one causes breaks _before and after_ itself. The
 *   right brace is not indented.
 *
 * * Semicolons ';' cause a line break after themselves.
 *
 * * Precedence is always resolved by enclosing a term in parentheses '()'.
 *
 * Since the above limitations likely make the pretty-printed code look far from
 * pretty, you are encouraged to _PATCH(1)_ this file with your own
 * implementations of some methods.
 */
|]

-- | Implementations of helper methods.
printerUtilImpl :: Doc
printerUtilImpl = unlinesToText [s|
#define IF_BAD_COERC(classname) \
    if (coercionLevel > reflection::CoercionLevel<classname>)

constexpr static const unsigned int INDENT [[maybe_unused]] = 4;

PrettyPrinter::PrettyPrinter(std::ostream& out, unsigned int indent,
                             int coercionLevel)
    : out(out), indent(indent), coercionLevel(coercionLevel) {}

void PrettyPrinter::NewLine(unsigned int indent) const {
    out << '\n' << std::string(indent, ' ');
}

void PrettyPrinter::NewLine() const { NewLine(indent); }

PrettyPrinter PrettyPrinter::WithCoercionLevel(int level) const {
    return {out, indent, level};
}

PrettyPrinter PrettyPrinter::Indented(unsigned int plusIndent,
                                      int coercionLevel) const {
    return {out, indent + plusIndent, coercionLevel};
}

PrettyPrinter PrettyPrinter::Dedented(unsigned int minusIndent,
                                      int coercionLevel) const {
    return {out, minusIndent > indent ? 0 : indent - minusIndent,
            coercionLevel};
}
|]

-- | Generates a method that prints objects of the given type.
makeMethod ::
     ListItemStorage  -- ^ How to access list elements.
  -> PrintableSymbol  -- ^ What object to print.
  -> Doc
makeMethod listItemStorage = \case
  PrintableNormalCategory s -> methodCategory s
  PrintableList listDesc    -> methodList listItemStorage listDesc
  PrintableFunctionRule r   -> methodFunctionRule r
  PrintableCustomToken t    -> methodCustomToken t
  PrintableIdent            -> methodIdent
  PrintableString           -> methodString
  PrintableInteger          -> methodInteger
  PrintableChar             -> methodChar
  PrintableDouble           -> methodDouble

-- | Generates implementations of overloaded @<<@ (Shift-Left) operators.
operatorShLImpl :: [PrintableSymbol] -> Doc
operatorShLImpl printables = unlinesToText [s|
#define PrettyPrinterSHL(type)                                               \
    const PrettyPrinter& operator<<(const PrettyPrinter& p, const type& v) { \
        p(v);                                                                \
        return p;                                                            \
    }
|] $++$ linesToText
    (map (\p -> "PrettyPrinterSHL(" ++ printableClassName p ++ ");")
      printables)
  $++$ unlinesToText [s|
const PrettyPrinter& operator<<(const PrettyPrinter& p, std::string_view v) {
    p.out << v;
    return p;
}
|]

------------------------------------------------------------------------
-- * Utility for code generation.
------------------------------------------------------------------------

-- | Internal structure; represents instructions to print an object.
data PrintTerm
  = Str         !String               -- ^ A literal string to print verbatim.
  | Nest        ![PrintTerm]          -- ^ Contents are to be printed indented.
  | Newline                           -- ^ Translated to a line break.
  | Nonterminal !NonterminalPrintTerm -- ^ A member field.

-- | Information about a printable field.
data NonterminalPrintTerm = NonterminalPrintTerm
  { printNonterm_coercionLevel :: !Integer  -- ^ The expected precedence.
  , printNonterm_fieldName     :: !String
  , printNonterm_isPointer     :: !Bool     -- ^ Is this a @std::unique_ptr@?
  }

-- | Step 0.
-- Generates printing instructions from a grammar rule's right-hand side.
getBasicPrintTerms ::
     CF.SentForm  -- ^ Representation of an AST class in the grammar.
  -> [PrintTerm]
getBasicPrintTerms sentForm = helper (map fst $ fieldNames sentForm) sentForm
  where
    helper fields = helper'
      where
        helper' = \case
          []       -> []
          h : tail -> case h of
            Left cat ->
              let f : fs = fields
              in Nonterminal (NonterminalPrintTerm
                { printNonterm_coercionLevel = case cat of
                    CF.CoercCat _ c -> c
                    _ -> 0
                , printNonterm_fieldName     = f
                , printNonterm_isPointer     = case cat of
                    CF.CoercCat _ _ -> True
                    CF.Cat      _   -> True
                    _               -> False
                }) : helper fs tail
            Right s  -> Str s : helper' tail

-- | Step 1.
-- Put sequences of terms enclosed in curly braces in v'Nest' terms.
handleCurlyBraces :: [PrintTerm] -> [PrintTerm]
handleCurlyBraces lst =
  let (read, unread) = helper lst
  in case unread of
    []               -> read
    unmatched : tail -> concat [read, [unmatched], handleCurlyBraces tail]
  where
    -- | Reads until the first unbalanced '}'.
    -- Returns (processed prefix, unprocessed suffix)
    helper :: [PrintTerm] -> ([PrintTerm], [PrintTerm])
    helper = \case
      []             -> ([], [])
      lst@(t : tail) -> case t of
        Str "{" ->
          let (inside, outside) = helper tail
          in case outside of
            closing@(Str "}"):tail ->
              let (read, unread) = helper tail
              in
                ( t : Nest (Newline : inside) : Newline
                  : closing : Newline : read
                , unread
                )
            _                      -> (t : inside, outside)
        Str "}" -> ([], lst)
        _       ->
          let (read, unread) = helper tail
          in (t : read, unread)

-- | Step 2.
-- Puts spaces between certain tokens (see 'disclaimer').
handleTokenSpacing :: [PrintTerm] -> [PrintTerm]
handleTokenSpacing = \case
  []           -> []
  first : tail -> first : helper first tail
  where
    helper :: PrintTerm -> [PrintTerm] -> [PrintTerm]
    helper = \case
      Str ";" -> (Newline :) . helper Newline
      prev    -> \case
        []         -> []
        cur : tail ->
          (if needSep prev cur then (space :) else id)
          (cur : helper cur tail)
    needSep :: PrintTerm -> PrintTerm -> Bool
    needSep a b = case a of
      Newline -> False
      Str s   -> (s `notElem` ["{", "[", "("]) && right
      _       -> right
      where
        right = case b of
          Newline -> False
          Str s   -> s `notElem` ["}", "]", ")", ",", ";"]
          _       -> True
    space = Str " "

-- | Step 3.
-- Merges consequent v'Str' terms into one.
-- Also removes empty strings from the result.
mergeStrs :: [PrintTerm] -> [PrintTerm]
mergeStrs = map (\case
    Left ss -> Str (concat ss)
    Right t -> t
  ) . helper
  where
    helper :: [PrintTerm] -> [Either [String] PrintTerm]
    helper = \case
      []       -> []
      h : tail -> let tail' = helper tail in
        case h of
          Str "" -> tail'
          Str sh -> case tail' of
            (Left ss) : ttail' -> Left (sh : ss) : ttail'
            _                  -> Left [sh] : tail'
          _      -> Right h : tail'

-- | Method that pretty-prints an @Ident@.
methodIdent :: Doc
methodIdent = unlinesToText [s|
void PrettyPrinter::operator()(const Ident& v) const {
    IF_BAD_COERC(Ident) out << '(';
    out << v.Value;
    IF_BAD_COERC(Ident) out << ')';
}
|]

-- | Method that pretty-prints a @String@ token.
methodString :: Doc
methodString = unlinesToText [s|
void PrettyPrinter::operator()(const String& v) const {
    IF_BAD_COERC(String) out << '(';
    PrintEscapedString(out, v.Value);
    IF_BAD_COERC(String) out << ')';
}
|]

-- | Method that pretty-prints an @Integer@ token.
methodInteger :: Doc
methodInteger = unlinesToText [s|
void PrettyPrinter::operator()(const Integer& v) const {
    IF_BAD_COERC(Integer) out << '(';
    out << v.Value;
    IF_BAD_COERC(Integer) out << ')';
}
|]

-- | Method that pretty-prints a @Double@ token.
methodDouble :: Doc
methodDouble = unlinesToText [s|
void PrettyPrinter::operator()(const Double& v) const {
    IF_BAD_COERC(Double) out << '(';
    PrintDouble(out, v.Value);
    IF_BAD_COERC(Double) out << ')';
}
|]

-- | Method that pretty-prints a @Char@ token.
methodChar :: Doc
methodChar = unlinesToText [s|
void PrettyPrinter::operator()(const Char& v) const {
    IF_BAD_COERC(Char) out << '(';
    PrintEscapedChar(out, v.Value);
    IF_BAD_COERC(Char) out << ')';
}
|]

-- | Generates a method that pretty-prints a value of the given nonterminal.
methodCategory ::
     String  -- ^ The category name.
  -> Doc
methodCategory name = linesToText
  [ "void PrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    std::visit(*this, v);"
  , "}"
  ]

-- | Generates a method that pretty-prints an object built from the given
-- grammar rule.
methodFunctionRule :: CF.Rule -> Doc
methodFunctionRule r = linesToText
  [ "void PrettyPrinter::operator()(const " ++ name
    ++ "& v [[maybe_unused]]) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  ] $+$ nest 4 (fst $ helperTerm2doc 0 terms)
  $+$ linesToText
  [ "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
  , "}"
  ]
  where
    name = CF.funName r
    terms = mergeStrs $ handleTokenSpacing $ handleCurlyBraces
      $ getBasicPrintTerms $ CF.rhsRule r
    -- | returns (Doc, Have we used the new printer object?)
    nestedTerm2doc :: Int -> [PrintTerm] -> (Doc, Bool)
    nestedTerm2doc lv terms =
      let (doc, uses) = helperTerm2doc lv terms
      in ((if uses
            then text ("PrettyPrinter printer" ++ show lv ++ " = " ++
              printerDotAtLv (lv - 1) ++ "Indented(INDENT);")
            else empty)
          $+$ doc
        , uses)
    -- | returns (Doc, Have we used the current printer object?)
    helperTerm2doc :: Int -> [PrintTerm] -> (Doc, Bool)
    helperTerm2doc lv = \case
      []          -> (empty, False)
      term : tail -> case term of
        Str s   ->
          (text ("out << " ++ show s ++ ";") $+$ taildoc, tailUsesPrinter)
        Newline -> (text (printerDot ++ "NewLine();") $+$ taildoc, True)
        Nest ns ->
          let (nestdoc, nestUsesPrinter) = nestedTerm2doc (lv + 1) ns
          in (text "{" $+$ nest 4 nestdoc $+$ text "}" $+$ taildoc
            , nestUsesPrinter || tailUsesPrinter)
        Nonterminal (NonterminalPrintTerm
          { printNonterm_coercionLevel = coerc
          , printNonterm_fieldName     = field
          , printNonterm_isPointer     = isPointer
          }) -> (text (concat
            [ printerDot
            , "WithCoercionLevel("
            , show coerc
            , ")("
            , if isPointer then "*v." else "v."
            , field
            , ");"
            ]) $+$ taildoc, True)
        where
          (taildoc, tailUsesPrinter) = helperTerm2doc lv tail
          printerDot = printerDotAtLv lv
    printerDotAtLv = \case
      0 -> ""
      i -> "printer" ++ show i ++ "."

-- | Generates a method that prints a custom token.
methodCustomToken :: String -> Doc
methodCustomToken name = linesToText
  [ "void PrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  , "    out << v." ++ tokenStorageName name ++ ";"
  , "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
  , "}"
  ]

-- | Generates a method that prints a list category.
methodList ::
     ListItemStorage           -- ^ How to access list elements.
  -> PrintableListDescription  -- ^ About the list.
  -> Doc
methodList listItemStorage (PrintableListDescription
  { printListName      = name
  , printListItemCoerc = itemcoerc
  , printListEmpty     = empty
  , printListCons      = cons
  , printListSingle    = single
  })
  = linesToText
  [ "void PrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  ] $+$ nest 4 body $+$ linesToText
  [ "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
  , "}"
  ]
  where
    maybeDereference = case listItemStorage of
      StoreByValue   -> ""
      StoreByPointer -> "*"
    body = case single of
      Nothing -> itemprinter
        $+$ text "for (const auto& item : v) {"
        $+$ nest 4 (lcons'
            $+$ text ("itemprinter(" ++ maybeDereference ++ "item);")
            $+$ mcons')
        $+$ text "}" $+$ cyclercons False
      Just (lsingle, _, rsingle) -> text "if (v.empty()) {"
        $+$ nest 4 empty'
        $+$ text "} else {"
        $+$ nest 4 (itemprinter
          $+$ text "auto last = std::prev(v.cend());"
          $+$ text "for (auto i = v.cbegin(); i != last; ++i) {"
          $+$ nest 4 (lcons'
              $+$ text ("itemprinter(" ++ maybeDereference ++ "*i);")
              $+$ mcons')
          $+$ text "}"
          $+$ lsingle'
          $+$ text ("itemprinter(" ++ maybeDereference ++ "*last);")
          $+$ rsingle'
          $+$ cyclercons True
        ) $+$ text "}"
        where
          lsingle' = compileSepString lsingle
          rsingle' = compileSepString rsingle

    itemprinter = text $ "PrettyPrinter itemprinter = WithCoercionLevel("
      ++ show itemcoerc ++ ");"
    compileSepString :: [String] -> Doc
    compileSepString strs =
      linesToText $ map printthis $ mergeStrs $ handleTokenSpacing
          $ getBasicPrintTerms $ map Right strs
      where
        printthis = \case
          Str s         -> "out << " ++ show s ++ ";"
          Newline       -> "NewLine();";
          Nest _        -> error "Somehow got nesting in list"
          Nonterminal _ -> error "Somehow got categories in separators"
    (lcons, mcons, rcons) = case cons of
      Nothing -> ([], [], [])
      Just (a, _, b, _, c) -> (a, b, c)
    lcons' = compileSepString $ lcons ++ [""]
    mcons' = compileSepString $ "" : (mcons ++ [""])
    rcons' = compileSepString $ "" : rcons
    empty' = case empty of
      Nothing -> text "// Empty list not defined in syntax"
      Just ss -> compileSepString ss
    cyclercons needDecr = if isEmpty rcons' then rcons' else
      text ("for (size_t i = v.size()"
        ++ (if needDecr then " - 1" else "") ++ "; i; --i) {")
      $+$ nest 4 rcons' $+$ text "}"
