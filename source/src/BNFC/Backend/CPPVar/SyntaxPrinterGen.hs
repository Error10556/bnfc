module BNFC.Backend.CPPVar.SyntaxPrinterGen
    ( syntaxPrinterHppFilename
    , syntaxPrinterCppFilename
    , makeSyntaxPrinter) where

import BNFC.Backend.CPPVar.CPPUtil
import qualified BNFC.CF
import qualified BNFC.Options
import Text.PrettyPrint
import BNFC.Backend.CPPVar.PrinterUtils
import Data.List

syntaxPrinterHppFilename :: String
syntaxPrinterHppFilename = "SyntaxPrinter.hpp"

syntaxPrinterCppFilename :: String
syntaxPrinterCppFilename = "SyntaxPrinter.cpp"

-- | -> (hpp, cpp)
makeSyntaxPrinter :: BNFC.Options.SharedOptions -> BNFC.CF.CF
    -> GroupedRules -> (Doc, Doc)
makeSyntaxPrinter opts cf groupedRules = (hpp, cpp)
    where
        hpp = linesToText
            [ "#pragma once"
            , "#include <iostream>"
            , "#include <string_view>"
            , "#include \"Absyn.hpp\""
            ] $++$ packwrap (printerClassDecl printable)
        cpp = text "#include \"SyntaxPrinter.hpp\""
            $++$ packwrap (printerImpl printable)
        packwrap = wrapPackage opts
        printable = getPrintableSymbols cf groupedRules

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
            NormalCategory name -> make name
            ListCategory {printListName = name} -> make name
            FunctionRule rule -> make $ BNFC.CF.funName rule
            Ident -> make BNFC.CF.catIdent
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
                ++ printableClassName sym ++  "& v) const {")
            $+$ nest 4 (makeMethodBody sym) $+$ text "}"
        makeMethodBody = \case
            NormalCategory _ -> text "std::visit(*this, v);"
            ListCategory {printListName=name} -> linesToText
                [ "PrintIndentForHeader();"
                , "size_t n = v.size();"
                , "out << \"" ++ name ++ " [\" << n << \"]\\n\";"
                , "if (!n) return;"
                , "if (n > 1) {"
                , "    SyntaxPrinter nonlast(this, true);"
                , "    size_t n1 = n - 1;"
                , "    for (size_t i = 0; i < n1; i++)"
                , "        std::visit(nonlast, v[i]);"
                , "}"
                , "std::visit(SyntaxPrinter(this, false), v.back());"
                ]
            Ident -> linesToText
                [ "PrintIndentForHeader();"
                , "out << \"Ident {\" << v.Value << \"}\\n\";"
                ]
            FunctionRule r -> linesToText
                [ "PrintIndentForHeader();"
                , "out << \"" ++ BNFC.CF.funName r ++ "\\n\";"
                ] $+$ case unsnoc (fieldNames $ BNFC.CF.rhsRule r) of
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
