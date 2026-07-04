{-# LANGUAGE QuasiQuotes #-}
module BNFC.Backend.CPPVar.SyntaxPrinterGen
  ( syntaxPrinterHppFilename
  , syntaxPrinterCppFilename
  , makeSyntaxPrinter) where

import BNFC.Backend.CPPVar.CPPUtil
import qualified BNFC.CF
import qualified BNFC.Options
import Text.PrettyPrint
import BNFC.Backend.CPPVar.PrinterUtils
import Data.String.QQ (s)
import BNFC.Backend.CPPVar.AbsynGen (tokenStorageName)

syntaxPrinterHppFilename :: String
syntaxPrinterHppFilename = "SyntaxPrinter.hpp"

syntaxPrinterCppFilename :: String
syntaxPrinterCppFilename = "SyntaxPrinter.cpp"

-- | -> (hpp, cpp)
makeSyntaxPrinter :: BNFC.Options.SharedOptions -> [PrintableSymbol]
  -> (Doc, Doc)
makeSyntaxPrinter opts printable = (hpp, cpp)
  where
    hpp = linesToText
      [ "#pragma once"
      , "#include <iostream>"
      , "#include <string_view>"
      , "#include \"Absyn.hpp\""
      ] $++$ packwrap (printerClassDecl printable)
    cpp = text "#include \"SyntaxPrinter.hpp\""
      $++$ text "#include \"PrinterCommon.hpp\""
      $++$ packwrap (printerImpl printable)
    packwrap = wrapPackage opts

printerClassDecl :: [PrintableSymbol] -> Doc
printerClassDecl symbols = linesToText
  [ "class SyntaxPrinter {"
  , "    std::ostream& out;"
  , "    bool currentIndentIsBranch;"
  , "    const SyntaxPrinter* maybeParent;"
  , "    SyntaxPrinter(const SyntaxPrinter* parent, " ++
    "bool currentIndentIsBranch);"
  , "    friend const SyntaxPrinter& operator<<(const SyntaxPrinter&,"
  , "                                           std::string_view);"
  , "    void PrintIndentForHeader() const;"
  , "    void PrintIndentAsIs() const;"
  , ""
  , "public:"
  , "    SyntaxPrinter(std::ostream& out);"
  ] $+$ nest 4 (foldr ($+$) empty (map makeMethod symbols))
  $+$ text "};"
  $++$ foldr ($+$) empty (map makeShiftL symbols)
  $+$ makeShiftLRaw "std::string_view"
  where
    makeMethodRaw s = text $ "void operator()(const " ++ s ++ "&) const;"
    makeMethod = makeMethodRaw . printableClassName
    makeShiftLRaw s = text $ concat
      ["const SyntaxPrinter& operator<<(const SyntaxPrinter&, ", s, ");"]
    makeShiftL = \case
      PrintableNormalCategory name -> make name
      PrintableList
        (PrintableListDescription {printListName = name}) -> make name
      PrintableFunctionRule rule -> make $ BNFC.CF.funName rule
      PrintableCustomToken name -> make name
      PrintableIdent -> make BNFC.CF.catIdent
      PrintableString -> make BNFC.CF.catString
      PrintableDouble -> make BNFC.CF.catDouble
      PrintableInteger -> make BNFC.CF.catInteger
      PrintableChar -> make BNFC.CF.catChar
      where
        make s = makeShiftLRaw $ concat ["const ", s, "&"]

printerImpl :: [PrintableSymbol] -> Doc
printerImpl symbols = linesToText
  [ "SyntaxPrinter::SyntaxPrinter(const SyntaxPrinter* parent,"
  , "                             bool currentIndentIsBranch)"
  , "    : out(parent->out),"
  , "      currentIndentIsBranch(currentIndentIsBranch),"
  , "      maybeParent(parent) {}"
  , ""
  , "SyntaxPrinter::SyntaxPrinter(std::ostream& out)"
  , "    : out(out), currentIndentIsBranch(false), maybeParent(nullptr) {}"
  , ""
  , "void SyntaxPrinter::PrintIndentForHeader() const {"
  , "    if (!maybeParent) return;"
  , "    maybeParent->PrintIndentAsIs();"
  , "    out << \"+-\";"
  , "}"
  , ""
  , "void SyntaxPrinter::PrintIndentAsIs() const {"
  , "    if (!maybeParent) return;"
  , "    maybeParent->PrintIndentAsIs();"
  , "    out << (currentIndentIsBranch ? \"| \" : \"  \");"
  , "}"
  ] $++$ vcatSpaced (map makeMethod symbols)
  $++$ linesToText
    [ "#define SyntaxPrinterSHL(type) \\"
    , "    const SyntaxPrinter& operator<<(" ++
      "const SyntaxPrinter& p, const type& v) \\"
    , "    { p(v); return p; }"
    ] $++$ linesToText
    ["SyntaxPrinterSHL(" ++ name ++ ");"
    | name <- map printableClassName symbols]
  $++$ linesToText
    [ "const SyntaxPrinter& operator<<(" ++
      "const SyntaxPrinter& p, std::string_view s) {"
    , "    p.out << s;"
    , "    return p;"
    , "}"
    ]
  where
    makeMethod sym = text
      ("void SyntaxPrinter::operator()(const "
        ++ printableClassName sym ++  "& v [[maybe_unused]]) const {")
      $+$ nest 4 (makeMethodBody sym) $+$ text "}"
    makeMethodBody = \case
      PrintableNormalCategory _ -> text "std::visit(*this, v);"
      PrintableList (PrintableListDescription {printListName=name}) -> linesToText
        [ "PrintIndentForHeader();"
        , "size_t n = v.size();"
        , "out << \"" ++ name ++ " [\" << n << \"]\\n\";"
        , "if (!n) return;"
        , "if (n > 1) {"
        , "    SyntaxPrinter nonlast(this, true);"
        , "    size_t n1 = n - 1;"
        , "    for (size_t i = 0; i < n1; i++)"
        , "        nonlast(v[i]);"
        , "}"
        , "SyntaxPrinter(this, false)(v.back());"
        ]
      PrintableCustomToken name -> stringlikePrint name
      PrintableIdent -> unlinesToText [s|
PrintIndentForHeader();
out << "Ident {" << v.Value << "}\n";
|]
      PrintableString -> stringlikePrint "String"
      PrintableInteger -> unlinesToText [s|
PrintIndentForHeader();
out << "Integer " << v.Value << '\n';
|]
      PrintableDouble -> unlinesToText [s|
PrintIndentForHeader();
out << "Double ";
PrintDouble(out, v.Value);
out << '\n';
|]
      PrintableChar -> unlinesToText [s|
PrintIndentForHeader();
out << "Char ";
PrintEscapedChar(out, v.Value);
out << '\n';
|]
      PrintableFunctionRule r -> linesToText
        [ "PrintIndentForHeader();"
        , "out << \"" ++ BNFC.CF.funName r ++ "\\n\";"
        ] $+$ case myUnsnoc (fieldNames $ BNFC.CF.rhsRule r) of
          Nothing -> empty
          Just (nonlasts, (lastName, lastCat)) -> (case nonlasts of
            [] -> empty
            _ -> text "SyntaxPrinter nonlast(this, true);"
              $+$ linesToText
              [concat
              ["nonlast(", if isPointerType cat then "*" else "",
              "v.", name, ");"] | (name, cat) <- nonlasts])
            $+$ text (concat
            [ "SyntaxPrinter(this, false)("
            , if isPointerType lastCat then "*" else ""
            , "v." , lastName, ");" ])
        where
          isPointerType = \case
            BNFC.CF.Cat _ -> True
            BNFC.CF.CoercCat _ _ -> True
            _ -> False
          myUnsnoc :: [a] -> Maybe ([a], a)
          myUnsnoc = \case
            [] -> Nothing
            item:tail -> case myUnsnoc tail of
              Nothing -> Just ([], item)
              Just (init, last) -> Just (item:init, last)
    stringlikePrint name = linesToText
      [ "PrintIndentForHeader();"
      , "out << \"" ++ name ++ " \";"
      , "PrintEscapedString(out, v." ++ tokenStorageName name ++ ");"
      , "out << '\\n';"
      ]
