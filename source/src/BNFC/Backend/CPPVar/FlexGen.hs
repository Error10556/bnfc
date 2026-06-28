{-# LANGUAGE QuasiQuotes #-}
module BNFC.Backend.CPPVar.FlexGen (flexFilename, makeFlex, scannerDecl) where

import Prelude hiding ((<>))
import BNFC.CF
import BNFC.Options
import Text.PrettyPrint (Doc, text, ($+$), empty, (<>))
import BNFC.PrettyPrint (Pretty(..))
import BNFC.Backend.CPPVar.FlexRegex as FReg
import BNFC.Backend.CPPVar.CPPUtil
import qualified Data.Map
import Data.Char (ord)
import BNFC.Utils (symbolToName, uncurry3)
import Data.Maybe (fromMaybe)
import Data.String.QQ (s)

flexFilename :: SharedOptions -> String
flexFilename = (++".l") . lang

-- | -> (The file contents, names of all tokens)
makeFlex :: SharedOptions -> CF -> (Doc, Data.Map.Map String String)
makeFlex opts cf = (flexHead opts cf
  $++$ bcommConditions
  $+$ literalTokenConditions cf
  $++$ literalTokenRegexDefs cf
  $++$ text "%%"
  $++$ bcommRules $++$ oneLineComments cf
  -- token rules here
  $++$ defImplicitTokens opts tkNames
  $++$ defString opts cf
  $++$ defChar opts cf
  $++$ defInteger opts cf
  $++$ defDouble opts cf
  $++$ defIdent opts cf
  $++$ text "<INITIAL>[\\t\\n\\f\\r\\x20]+ /* whitespace */;"
  $+$ text ("<INITIAL><<EOF>> return " ++ bisonParserName opts
    ++ "::make_YYEOF();")
  $++$ text ("[\\x00-\\xff] return " ++ bisonParserName opts
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

literalTokenConditions :: CF -> Doc
literalTokenConditions cf =
  (if BNFC.CF.catString `elem` BNFC.CF.cfgLiterals cf
    then text "%s STRING ESCAPE" else empty)
  $+$ (if BNFC.CF.catChar `elem` BNFC.CF.cfgLiterals cf
    then text "%s CHAR" else empty)

literalTokenUtils :: CF -> Doc
literalTokenUtils cf =
  (if catChar `elem` cfgLiterals cf then (unlinesToText [s|
inline int hexDigitValue(char ch) {
    if ('0' <= ch && ch <= '9') return ch - '0';
    if ('a' <= ch && ch <= 'f') return ch - 'a' + 10;
    if ('A' <= ch && ch <= 'F') return ch - 'A' + 10;
    return 0;
}

inline int32_t hexInt32(const char* start, int len) {
    int32_t val = 0;
    for (int i = 0; i < len; i++) val = (val << 4) | hexDigitValue(start[i]);
    return val;
}

inline int32_t minCharForEncodedLen(int len) {
    if (len < 2) return 0;
    if (len == 2) return 0x80;
    return static_cast<int32_t>(1) << (11 + 5 * (len - 3));
}

    /* returns -1 if a character uses a non-minimal # of bytes */
inline int decodeUTF8(const char* start, int len) {
    if (len == 1) return *start;
    int firstbits = 7 - len;
    int32_t res = *start & ((1 << firstbits) - 1);
    for (int i = 1; i < len; i++) res = (res << 6) | (start[i] & 0x3f);
    return res >= minCharForEncodedLen(len) ? res : -1;
}

inline int32_t bitSegment(int shiftr, int masklen, int32_t val) {
    return (val >> shiftr) & ((static_cast<int32_t>(1) << masklen) - 1);
}
|]) else empty)

  $++$ (if catString `elem` cfgLiterals cf then (unlinesToText [s|
inline void encodeUTF8(std::string& dest, int32_t ch) {
    if (ch < 0) {
        dest.push_back(0xFF);
        return;
    }
    if (ch <= 0x7F) {
        dest.push_back(static_cast<char>(ch));
        return;
    }
    char buf[7];
    int sz = 0;
    while (ch) {
        buf[sz++] = ch & 0x3F;
        ch >>= 6;
    }
    buf[sz] = 0;
    sz += (static_cast<int>(buf[sz - 1]) >= 1 << (8 - sz - 1));
    buf[sz] = 0;
    for (int i = 0, j = sz - 1; i < j; ++i, --j) std::swap(buf[i], buf[j]);
    buf[0] |= static_cast<char>(0xFF << (8 - sz));
    for (int i = 1; i < sz; i++) buf[i] |= 0x80;
    dest.append(buf);
}
|]) else empty)

literalTokenRegexDefs :: CF -> Doc
literalTokenRegexDefs cf = let
    hasChar = BNFC.CF.catChar `elem` BNFC.CF.cfgLiterals cf
  in (if BNFC.CF.catString `elem` BNFC.CF.cfgLiterals cf || hasChar
    then text "HEXINT [0-7][0-9a-fA-F]{7}|[0-9a-fA-F]{1,7}" else empty)
  $++$ (if hasChar then linesToText
    [ "    /* 2-6 bytes. Does not handle the [\\x00-\\x7F] case."
    , "     * This regex permits non-minimal-length encodings,"
    , "     * but they are rejected in decodeUTF8."
    , "     */"
    , "UTF8MULTICHAR [\\xC0-\\xDF][\\x80-\\xBF]|[\\xE0-\\xEF][\\x80-\\xBF]{2}"
      ++ "|[\\xF0-\\xF7][\\x80-\\xBF]{3}|[\\xF8-\\xFB][\\x80-\\xBF]{4}"
      ++ "|[\\xFC-\\xFD][\\x80-\\xBF]{5}"
    ] else empty)

defIdent :: SharedOptions -> CF -> Doc
defIdent opts cf = if catIdent `elem` cfgLiterals cf
  then text $ "<INITIAL>[a-zA-Z_][a-zA-Z0-9_]* return "
    ++ bisonParserName opts ++ "::make_IDENT(yytext);"
  else empty

defString :: SharedOptions -> CF -> Doc
defString opts cf = if catString `elem` cfgLiterals cf
  then (unlinesToText [s|
    /* String */
<INITIAL>\" BEGIN(STRING); yyextra->clear();
|] $+$ text ("<STRING>\\\" BEGIN(INITIAL); return "
    ++ bisonParserName opts ++ "::make_STRING(*yyextra);")
  $+$ [s|
<STRING>\\ BEGIN(ESCAPE);
<STRING>. yyextra->push_back(*yytext);
<ESCAPE>0 BEGIN(STRING); yyextra->push_back('\0');
<ESCAPE>a BEGIN(STRING); yyextra->push_back('\a');
<ESCAPE>b BEGIN(STRING); yyextra->push_back('\b');
<ESCAPE>f BEGIN(STRING); yyextra->push_back('\f');
<ESCAPE>n BEGIN(STRING); yyextra->push_back('\n');
<ESCAPE>r BEGIN(STRING); yyextra->push_back('\r');
<ESCAPE>t BEGIN(STRING); yyextra->push_back('\t');
<ESCAPE>v BEGIN(STRING); yyextra->push_back('\v');
<ESCAPE>x{HEXINT} {
        BEGIN(STRING);
        encodeUTF8(*yyextra, hexInt32(yytext + 1, yyleng - 1));
    }
<ESCAPE>. BEGIN(STRING); yyextra->push_back(*yytext);
|]) else empty

defDouble :: SharedOptions -> CF -> Doc
defDouble opts cf = if catDouble `elem` cfgLiterals cf
  then (unlinesToText [s|
    /* Double */
<INITIAL>[+\-]?[0-9]+(\.[0-9]+)?([eE][+\-]?[0-9]+)? {
        const char* const start = yytext + (*yytext == '+');
        const char* const end = yytext + yyleng;
        double num;
        auto res = std::from_chars(start, end, num);
        if (res.ec == std::errc() && res.ptr == end)
|] $+$ linesToText
    [ "            return " ++ bisonParserName opts ++ "::make_DOUBLE(num);"
    , "        else"
    , "            return " ++ bisonParserName opts ++ "::make_YYerror();"
    , "    }"
    ]) else empty

defInteger :: SharedOptions -> CF -> Doc
defInteger opts cf = if catInteger `elem` cfgLiterals cf
  then (unlinesToText [s|
    /* Integer (must be above Double) */
<INITIAL>[+\-]?[0-9]+ {
        const char* const start = yytext + (*yytext == '+');
        const char* const end = yytext + yyleng;
        long num;
        auto res = std::from_chars(start, end, num);
        if (res.ec == std::errc() && res.ptr == end)
|] $+$ linesToText
    [ "            return " ++ bisonParserName opts ++ "::make_INTEGER(num);"
    , "        else"
    , "            return " ++ bisonParserName opts ++ "::make_YYerror();"
    , "    }"
    ]) else empty

defChar :: SharedOptions -> CF -> Doc
defChar opts cf = if catChar `elem` cfgLiterals cf
  then linesToText (
    [ "    /* Char in UTF-8 */"
    , "<INITIAL>' BEGIN(CHAR);"
    ] ++ map simpleEscape "0abfnrtv" ++
    [ "<CHAR>\\\\x{HEXINT}' {"
    , "        BEGIN(INITIAL);"
    , "        return " ++ bisonParserName opts
      ++ "::make_CHAR(hexInt32(yytext + 2, yyleng - 2));"
    , "    }"
    , "<CHAR>\\\\({UTF8MULTICHAR}|.)' {"
    , "        BEGIN(INITIAL);"
    , "        int32_t charcode = decodeUTF8(yytext + 1, yyleng - 2);"
    , "        if (charcode == -1) return " ++ bisonParserName opts
      ++ "::make_YYerror();"
    , "        return " ++ bisonParserName opts
      ++ "::make_CHAR(charcode);"
    , "    }"
    , "<CHAR>({UTF8MULTICHAR}|[^'\\\\\\n])' {"
    , "        BEGIN(INITIAL);"
    , "        int32_t charcode = decodeUTF8(yytext, yyleng - 1);"
    , "        if (charcode == -1) return " ++ bisonParserName opts
      ++ "::make_YYerror();"
    , "        return " ++ bisonParserName opts ++ "::make_CHAR(charcode);"
    , "    }"
    ]) else empty
  where
    simpleEscape ch = concat
      [ "<CHAR>\\\\", [ch] , "' BEGIN(INITIAL); return "
      , bisonParserName opts , "::make_CHAR('\\" , [ch], "');"]

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

flexHead :: SharedOptions -> CF -> Doc
flexHead opts cf = (linesToText $
  [ "%option warn nodefault"
  , "%option 8bit reentrant"
  , "%option noyywrap noinput nounput"
  ] ++ (case inPackage opts of
    Nothing -> []
    Just namespace -> ["%option prefix=\"" ++ namespace ++ "\""]) ++
  [ "%option outfile=\"" ++ lang opts ++ ".lex.cpp\""
  , ""
  , "%{"
  ] ++ (if useCharconv then ["#include <charconv>"] else []) ++
  [ "#include <string>"
  , "#include <string_view>"
  , "#include <system_error>"
  , "#include \"" ++ lang opts ++ ".tab.hpp\""
  , ""
  , "#define YY_DECL " ++ bisonParserName opts
    ++ "::symbol_type " ++ maybePrefix ++ "lex(yyscan_t yyscanner)"
  ]) $++$ literalTokenUtils cf $++$ linesToText
  [ "%}"
  , ""
  , "%option extra-type=\"std::string*\""
  ]
  where
    useCharconv = catInteger `elem` cfgLiterals cf
      || catDouble `elem` cfgLiterals cf
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
  ( linesToText . map (("%s COMMENT"++) . show) $ [1..length docs]
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
        -- multibyte/multicharacter ending => handle endhead
        FReg.Concat _ _ -> text ("<COMMENT" ++ istr ++ ">")
          <> pretty endhead <> text " ;"
        -- singlebyte ending
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
