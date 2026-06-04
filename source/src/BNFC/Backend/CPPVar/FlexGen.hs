{- HLINT ignore "Use zipWith" -}
module BNFC.Backend.CPPVar.FlexGen (flexFilename, makeFlex, scannerDecl) where

import BNFC.CF
import BNFC.Options
import Text.PrettyPrint (Doc, text, ($+$), empty)
import BNFC.PrettyPrint (Pretty(..))
import BNFC.Backend.CPPVar.FlexRegex as FReg
import BNFC.Backend.CPPVar.CPPUtil
import qualified Data.Map
import Data.Char (ord)
import BNFC.Utils (symbolToName, uncurry3)
import Data.Maybe (fromMaybe)

flexFilename :: SharedOptions -> String
flexFilename = (++".l") . lang

-- | -> (The file contents, names of all tokens)
makeFlex :: SharedOptions -> CF -> (Doc, Data.Map.Map String String)
makeFlex opts cf = (flexHead opts
    $++$ bcommConditions
    $++$ text "%%"
    $++$ bcommRules $++$ oneLineComments cf
    -- $++$ defined token rules
    $++$ defImplicitTokens opts tkNames
    $++$ defString opts cf
    $+$ defDouble opts cf
    $+$ defInteger opts cf
    $+$ defChar opts cf
    $+$ defIdent opts cf
    $++$ text "<INITIAL>[\\t\\n\\f\\r\\x20]+ /* whitespace */;"
    $+$ text ("<INITIAL><<EOF>> return " ++ bisonParserName opts
        ++ "::make_YYEOF();")
    $+$ text ("<INITIAL>. return " ++ bisonParserName opts
        ++ "::make_YYerror();")
    $++$ text "%%"
    $++$ maybeWrapNamespace (scannerDecl opts $++$ scannerImpl opts)
    , tkNames)
    where
        (bcommConditions, bcommRules) = commentBlocks cf
        tkNames = nameAllTokens cf
        maybeWrapNamespace = maybe id wrapNamespace (inPackage opts)

defImplicitTokens :: SharedOptions -> Data.Map.Map String String -> Doc
defImplicitTokens opts mp = linesToText
    ["<INITIAL>" ++ show (pretty (flexConcatUTF8 str))
        ++ " return " ++ bisonParserName opts
        ++ "::make_" ++ name ++ "();" | (str, name) <- Data.Map.toList mp]

bisonParserName :: SharedOptions -> String
bisonParserName opts = case inPackage opts of
    Nothing -> "Parser"
    Just ns -> ns ++ "::Parser"

-- | empty if unused
defIdent :: SharedOptions -> CF -> Doc
defIdent opts cf = if TokenCat catIdent `elem` cfgUsedCats cf
    then text $ "<INITIAL>[a-zA-Z_][a-zA-Z0-9_]* return "
        ++ bisonParserName opts ++ "::make_IDENT(yytext);"
    else empty

defString :: SharedOptions -> CF -> Doc
defString _ cf = if TokenCat catString `elem` cfgUsedCats cf
    then error "String token is unsupported at the moment"
    else empty

defDouble :: SharedOptions -> CF -> Doc
defDouble _ cf = if TokenCat catDouble `elem` cfgUsedCats cf
    then error "Double token is unsupported at the moment"
    else empty

defInteger :: SharedOptions -> CF -> Doc
defInteger _ cf = if TokenCat catInteger `elem` cfgUsedCats cf
    then error "Integer token is unsupported at the moment"
    else empty

defChar :: SharedOptions -> CF -> Doc
defChar _ cf = if TokenCat catChar `elem` cfgUsedCats cf
    then error "Char token is unsupported at the moment"
    else empty

nameAllTokens :: CF -> Data.Map.Map String String
nameAllTokens cf = helper 1 (cfgKeywords cf ++ cfgSymbols cf)
    where
        helper :: Int -> [String] -> Data.Map.Map String String
        helper i ss = case ss of
            [] -> Data.Map.empty
            s:tail -> case symbolToName s of
                Just name -> Data.Map.insert s name $ helper i tail
                Nothing ->
                    if isCIdent s
                    then Data.Map.insert s ("KW_" ++ s) (helper i tail)
                    else Data.Map.insert s ("SYM_" ++ show i)
                        (helper (i + 1) tail)

isCIdent :: String -> Bool
isCIdent = \case
    "" -> False
    ch : tail -> alpha ch && all (\ch -> alpha ch || digit ch) tail
        where
            alpha ch = ch == '_' || (ord 'A' <= o && o <= ord 'Z')
                || (ord 'a' <= o && o <= ord 'z')
                where o = ord ch
            digit ch = ord '0' <= o && o <= ord '9'
                where o = ord ch

flexHead :: SharedOptions -> Doc
flexHead opts = linesToText $
    [ "%option warn nodefault"
    , "%option 8bit reentrant"
    , "%option noyywrap"
    ] ++ (case inPackage opts of
        Nothing -> []
        Just namespace -> ["%option prefix=\"" ++ namespace ++ "\""]) ++
    [ "%option outfile=\"" ++ lang opts ++ ".lex.cpp\""
    , ""
    , "%{"
    , "/* Compile " ++ lang opts ++ ".l with:"
    , " * $ flex " ++ lang opts ++ ".l"
    , " * Compile " ++ lang opts ++ ".lex.cpp with:"
    , " * $ c++ -std=c++17 " ++ lang opts ++ ".lex.cpp -c -o "
        ++ lang opts ++ ".lex.o"
    , " */"
    , "#include <string>"
    , "#include <system_error>"
    , "#include <string_view>"
    , "#include \"" ++ lang opts ++ ".tab.hpp\""
    , ""
    , "#define YY_DECL " ++ bisonParserName opts
        ++ "::symbol_type " ++ maybePrefix ++ "lex(yyscan_t yyscanner)"
    , ""
    , "%}"
    , ""
    , "%option extra-type=\"std::string*\""
    ]
    where
        maybePrefix = case inPackage opts of
            Nothing -> ""
            Just s -> s

oneLineComments :: CF -> Doc
oneLineComments cf = foldr ($+$) empty
    [text "<INITIAL>" <> pretty (flexConcatUTF8 s) <> text ".* ;"
        | CommentS s <- cfgPragmas cf]

-- | (start condition declarations, rules)
commentBlocks :: CF -> (Doc, Doc)
commentBlocks cf =
    ( linesToText . map (("%x COMMENT"++) . show) $ [1..length docs]
    , vcatSpaced docs )
    where
        makeRules :: Int -> String -> String -> Doc
        makeRules i start end =
            text "<INITIAL>"
                <> pretty (FReg.flexConcatUTF8 start)
                <> text (" BEGIN(COMMENT" ++ istr ++ ");")
            $+$ text ("<COMMENT" ++ istr ++ ">")
                <> pretty endregex <> text " BEGIN(INITIAL);"
            $+$ text ("<COMMENT" ++ istr ++ ">[^")
                <> pretty endhead
                <> text "]+ ;"
            $+$ case endregex of
                -- | multibyte/multicharacter ending => handle endhead
                FReg.Concat _ _ -> text ("<COMMENT" ++ istr ++ ">")
                    <> pretty endhead <> text " ;"
                -- | singlebyte ending
                _ -> empty
            where
                istr = show i
                endregex = FReg.flexConcatUTF8 end
                endhead = case endregex of
                    FReg.Concat b@(FReg.Onebyte _) _ -> b
                    b@(FReg.Onebyte _) -> b
                    _ -> error "empty ending in a block comment"
        docs = map (uncurry3 makeRules) $ uncurry (zip3 [1..]) . unzip
            $ [se | CommentM se <- cfgPragmas cf]

scannerDecl :: SharedOptions -> Doc
scannerDecl opts = linesToText
    [ "class " ++ name ++ " {"
    , "    yyscan_t scanner;"
    , "    " ++ name ++ "();"
    , ""
    , "public:"
    , "    " ++ name ++ "(FILE* file);"
    , "    " ++ name ++ "(std::string_view str);"
    , "    yyscan_t FlexScanner() const;"
    , "    ~" ++ name ++ "();"
    , "};"
    ]
    where
        name = case inPackage opts of
            Nothing -> "Scanner"
            Just ns -> ns ++ "Scanner"

scannerImpl :: SharedOptions -> Doc
scannerImpl opts = linesToText
    [ name ++ "::" ++ name ++ "() {"
    , "    int err = " ++ prefix ++ "lex_init_extra(new std::string(), &scanner);"
    , "    if (err) throw std::system_error(err, std::generic_category(),"
    , "        \"Cannot create scanner\");"
    , "}"
    , ""
    , name ++ "::" ++ name ++ "(FILE* file) : " ++ name ++ "() {"
    , "    " ++ prefix ++ "restart(file, scanner);"
    , "}"
    , ""
    , name ++ "::" ++ name ++ "(std::string_view str) : " ++ name ++ "() {"
    , "    " ++ prefix ++ "_scan_bytes(str.data(), str.size(), scanner);"
    , "}"
    , ""
    , "yyscan_t " ++ name ++ "::FlexScanner() const { return scanner; }"
    , ""
    , name ++ "::~" ++ name ++ "() {"
    , "    delete " ++ prefix ++ "get_extra(scanner);"
    , "    " ++ prefix ++ "lex_destroy(scanner);"
    , "}"
    ]
    where
        name = case inPackage opts of
            Nothing -> "Scanner"
            Just ns -> ns ++ "Scanner"
        prefix = fromMaybe "yy" (inPackage opts)
