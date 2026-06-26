{-# LANGUAGE QuasiQuotes #-}

module BNFC.Backend.CPPVar.PrettyPrinterGen
  ( prettyPrinterHppFilename
  , prettyPrinterCppFilename
  , makePrettyPrinter) where

import Text.PrettyPrint hiding (Str)
import BNFC.Backend.CPPVar.CPPUtil
import BNFC.Backend.CPPVar.PrinterUtils
import Data.String.QQ
import qualified BNFC.Options
import qualified BNFC.CF

prettyPrinterHppFilename :: String
prettyPrinterHppFilename = "PrettyPrinter.hpp"

prettyPrinterCppFilename :: String
prettyPrinterCppFilename = "PrettyPrinter.cpp"

-- | -> (hpp, cpp)
makePrettyPrinter :: BNFC.Options.SharedOptions -> [PrintableSymbol]
  -> (Doc, Doc)
makePrettyPrinter opts printable = (hpp, cpp)
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

#include "Absyn.hpp"
|] $++$ packwrap (printerUtilImpl
      $++$ vcatSpaced (map makeMethod printable) $++$ operatorShLImpl printable)

    packwrap = wrapPackage opts

printerClassDecl :: [PrintableSymbol] -> Doc
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

printerUtilImpl :: Doc
printerUtilImpl = unlinesToText [s|
#define IF_BAD_COERC(classname) \
    if (coercionLevel > reflection::CoercionLevel<classname>)

constexpr static const unsigned int INDENT = 4;

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

data PrintTerm
  = Str String
  | Nest [PrintTerm]
  | Newline
  | Nonterminal Integer String Bool  -- ^ coercionLevel, fieldName, isPointer

-- | Step 0
getBasicPrintTerms :: BNFC.CF.SentForm -> [PrintTerm]
getBasicPrintTerms sentForm = helper (map fst $ fieldNames sentForm) sentForm
  where
    helper fields = helper'
      where
        helper' = \case
          [] -> []
          h:tail -> case h of
            Left cat -> let f:fs = fields in
              Nonterminal
              (case cat of BNFC.CF.CoercCat _ c -> c; _ -> 0) f
              (isPointerCat cat)
              : helper fs tail
            Right s -> Str s : helper' tail
    isPointerCat = \case
      BNFC.CF.CoercCat _ _ -> True
      BNFC.CF.Cat _ -> True
      _ -> False

-- | Step 1
handleCurlyBraces :: [PrintTerm] -> [PrintTerm]
handleCurlyBraces lst = let (read, unread) = helper lst
  in case unread of
    [] -> read
    unmatched:tail -> concat [read, [unmatched], handleCurlyBraces tail]
  where
    -- | reads until the first unbalanced '}'
    --  -> (processed, unprocessed suffix)
    helper :: [PrintTerm] -> ([PrintTerm], [PrintTerm])
    helper = \case
      [] -> ([], [])
      lst@(t:tail) -> case t of
        Str "{" -> let (inside, outside) = helper tail
          in case outside of
            closing@(Str "}"):tail -> let
                (read, unread) = helper tail
              in (t:Nest (Newline:inside):Newline
                :closing:Newline:read, unread)
            _ -> (t:inside, outside)
        Str "}" -> ([], lst)
        _ -> let (read, unread) = helper tail in (t:read, unread)

-- | Step 2
handleTokenSpacing :: [PrintTerm] -> [PrintTerm]
handleTokenSpacing = \case
  [] -> []
  first:tail -> first : helper first tail
  where
    helper :: PrintTerm -> [PrintTerm] -> [PrintTerm]
    helper = \case
      Str ";" -> (Newline:) . helper Newline
      prev -> \case
        [] -> []
        cur:tail -> (if needSep prev cur then (space:) else id)
          (cur:helper cur tail)
    needSep :: PrintTerm -> PrintTerm -> Bool
    needSep a b = case a of
      Newline -> False
      Str s -> (s `notElem` ["{", "[", "("]) && right
      _ -> right
      where
        right = case b of
          Newline -> False
          Str s -> s `notElem` ["}", "]", ")", ",", ";"]
          _ -> True
    space = Str " "

-- | Step 3
-- also removes empty strings from the result
mergeStrs :: [PrintTerm] -> [PrintTerm]
mergeStrs = map (\case Left ss -> Str (concat ss); Right t -> t) . helper
  where
    helper = \case
      [] -> []
      h:tail -> let tail' = helper tail in
        case h of
          Str "" -> tail'
          Str sh -> case tail' of
            (Left ss):ttail' -> Left (sh:ss):ttail'
            _ -> Left [sh] : tail'
          _ -> Right h : tail'

methodIdent :: Doc
methodIdent = unlinesToText [s|
void PrettyPrinter::operator()(const Ident& v) const {
    IF_BAD_COERC(Ident) out << '(';
    out << v.Value;
    IF_BAD_COERC(Ident) out << ')';
}
|]

methodString :: Doc
methodString = unlinesToText

methodCategory :: String -> Doc
methodCategory name = linesToText
  [ "void PrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    std::visit(*this, v);"
  , "}"
  ]

methodFunctionRule :: BNFC.CF.Rule -> Doc
methodFunctionRule r = linesToText
  [ "void PrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  ] $+$ nest 4 (fst $ helperTerm2doc 0 terms)
  $+$ linesToText
  [ "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
  , "}"
  ]
  where
    name = BNFC.CF.funName r
    terms = mergeStrs . handleTokenSpacing . handleCurlyBraces
      . getBasicPrintTerms $ BNFC.CF.rhsRule r
    -- | -> (Doc, have we used the new printer object?)
    nestedTerm2doc :: Int -> [PrintTerm] -> (Doc, Bool)
    nestedTerm2doc lv terms =
      let (doc, uses) = helperTerm2doc lv terms
      in ((if uses then
        text ("PrettyPrinter printer" ++ show lv ++ " = " ++
          printerDotAtLv (lv - 1) ++ "Indented(INDENT);") else empty)
        $+$ doc, uses)
    -- | -> (Doc, have we used the current printer object?)
    helperTerm2doc :: Int -> [PrintTerm] -> (Doc, Bool)
    helperTerm2doc lv = \case
      [] -> (empty, False)
      term:tail -> case term of
        Str s -> ( text ("out << " ++ show s ++ ";") $+$ taildoc
             , tailUsesPrinter)
        Newline -> (text (printerDot ++ "NewLine();") $+$ taildoc, True)
        Nest ns ->
          let (nestdoc, nestUsesPrinter) = nestedTerm2doc (lv + 1) ns
          in ( text "{" $+$ nest 4 nestdoc $+$ text "}" $+$ taildoc
            , nestUsesPrinter || tailUsesPrinter)
        Nonterminal coerc field isPointer -> (text (concat
          [ printerDot, "WithCoercionLevel("
          , show coerc, ")(", if isPointer then "*v." else "v."
          , field, ");"]) $+$ taildoc, True)
        where
          (taildoc, tailUsesPrinter)
            = helperTerm2doc lv tail
          printerDot = printerDotAtLv lv
    printerDotAtLv = \case
      0 -> ""
      i -> "printer" ++ show i ++ "."

methodList :: String -> Integer
  -> Maybe [String]
  -> Maybe ([String], BNFC.CF.Cat, [String], BNFC.CF.Cat, [String])
  -> Maybe ([String], BNFC.CF.Cat, [String])
  -> Doc
methodList name itemcoerc empty cons single = linesToText
  [ "void PrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  ] $+$ nest 4 body $+$ linesToText
  [ "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
  , "}"
  ]
  where
    body = case single of
      Nothing -> itemprinter
        $+$ text "for (const auto& item : v) {"
        $+$ nest 4 (lcons' $+$ text "itemprinter(item);"
          $+$ mcons')
        $+$ text "}" $+$ cyclercons False
      Just (lsingle, _, rsingle) -> text "if (v.empty()) {"
        $+$ nest 4 empty'
        $+$ text "} else {"
        $+$ nest 4 (itemprinter
          $+$ text "auto last = std::prev(v.cend());"
          $+$ text "for (auto i = v.cbegin(); i != last; ++i) {"
          $+$ nest 4 (lcons' $+$ text "itemprinter(*i);" $+$ mcons')
          $+$ text "}"
          $+$ lsingle' $+$ text "itemprinter(*last);" $+$ rsingle'
          $+$ cyclercons True
        ) $+$ text "}"
        where
          lsingle' = compileSepString lsingle
          rsingle' = compileSepString rsingle

    itemprinter = text $ "PrettyPrinter itemprinter = WithCoercionLevel("
      ++ show itemcoerc ++ ");"
    compileSepString :: [String] -> Doc
    compileSepString strs =
      linesToText . map printthis . mergeStrs . handleTokenSpacing
          . getBasicPrintTerms . map Right $ strs
      where
        printthis = \case
          Str s -> "out << " ++ show s ++ ";"
          Newline -> "NewLine();";
          Nest _ -> error "Somehow got nesting in list"
          Nonterminal _ _ _ ->
            error "Somehow got categories in separators"
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

makeMethod :: PrintableSymbol -> Doc
makeMethod = \case
  NormalCategory s -> methodCategory s
  ListCategory
    { printListName = name
    , printListItemCoerc = itemcoerc
    , printListEmpty = empty
    , printListCons = cons
    , printListSingle = single
    } -> methodList name itemcoerc empty cons single
  FunctionRule r -> methodFunctionRule r
  Ident -> methodIdent
  String -> methodString
  Integer -> methodInteger
  Char -> methodChar
  Double -> methodDouble

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
