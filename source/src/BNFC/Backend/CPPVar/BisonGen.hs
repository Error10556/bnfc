module BNFC.Backend.CPPVar.BisonGen (bisonFilename, makeBison) where

import qualified BNFC.Options
import qualified BNFC.CF
import BNFC.Backend.CPPVar.FlexGen (scannerDecl)
import qualified Data.Map
import qualified Data.Set
import Text.PrettyPrint
import BNFC.Backend.CPPVar.CPPUtil
import Data.List (intercalate)
import BNFC.CF (CFG(cfgPragmas))

bisonFilename :: BNFC.Options.SharedOptions -> String
bisonFilename opts = BNFC.Options.lang opts ++ ".ypp"

makeBison :: BNFC.Options.SharedOptions -> BNFC.CF.CF
  -> Data.Map.Map String String -> GroupedRules -> Doc
makeBison opts cf implicitTokenNames groupedRules =
  bisonHeader opts
  $++$ tokenDefs implicitTokenNames cf
  $++$ codeRequires utils entrypoints
  $++$ nonterms groupedRules
  $++$ codeProvides utils entrypoints
  $++$ codeLex utils
  $++$ text "%start __start__"
  $++$ text "%%"
  $++$ startRules entrypoints
  $++$ vcatSpaced (map (uncurry $ category implicitTokenNames)
      $ Data.Map.toList groupedRules)
  $++$ text "%%"
  $++$ codeSection utils opts entrypoints
  where
    utils = newBisonUtils opts
    entrypoints = extractEntrypoints cf groupedRules

extractEntrypoints :: BNFC.CF.CF -> GroupedRules -> [BNFC.CF.Cat]
extractEntrypoints cf grouped = merge
  $ if null res then Data.Map.keys grouped else res
  where
    res = concat [map BNFC.CF.wpThing cats
      | BNFC.CF.EntryPoints cats <- BNFC.CF.cfgPragmas cf]
    merge = Data.Set.toList . Data.Set.fromList . map (\case
      BNFC.CF.CoercCat s _ -> BNFC.CF.Cat s
      other -> other)

bisonHeader :: BNFC.Options.SharedOptions -> Doc
bisonHeader opts = linesToText
  [ "%require \"3.2\""
  , "%language \"c++\""
  , "%define api.value.type variant"
  , "%define api.token.constructor"
  , "%define api.parser.class {Parser}"
  ]
  $+$ case BNFC.Options.inPackage opts of
    Nothing -> empty
    Just ns -> text ("%define api.namespace {" ++ ns ++ "}")
  $+$ linesToText
  [ "%lex-param {yyscan_t scanner}"
  , "%parse-param {yyscan_t scanner}"
  , "%parse-param {std::optional<std::variant<ParseResultVariant, "
    ++ "syntax_error>>* result}"
  , "%header \"" ++ BNFC.Options.lang opts ++ ".tab.hpp\""
  ]

data BisonUtils = BisonUtils
  { inPackage :: Bool
  , namespaceWrap :: Doc -> Doc
  , namespaceNameOrEmpty :: String
  , namespacePrefix :: String
  }

newBisonUtils :: BNFC.Options.SharedOptions -> BisonUtils
newBisonUtils opts = case BNFC.Options.inPackage opts of
  Nothing -> BisonUtils
    { inPackage = False
    , namespaceWrap = id
    , namespaceNameOrEmpty = ""
    , namespacePrefix = ""
    }
  Just ns -> BisonUtils
    { inPackage = True
    , namespaceWrap = bisonBraces ("namespace " ++ ns)
    , namespaceNameOrEmpty = ns
    , namespacePrefix = ns ++ "::"
    }

data LiteralTokenInfo = LiteralTokenInfo
  { literalTokenName :: String
  , literalTokenStorageType :: String
  }

literalTokenInfo :: Data.Map.Map String LiteralTokenInfo
literalTokenInfo = Data.Map.fromList
  [ (BNFC.CF.catIdent, LiteralTokenInfo
      { literalTokenName = "IDENT"
      , literalTokenStorageType = "std::string" })
  , (BNFC.CF.catChar, LiteralTokenInfo
      { literalTokenName = "CHAR"
      , literalTokenStorageType = "int32_t" })
  , (BNFC.CF.catInteger, LiteralTokenInfo
      { literalTokenName = "INTEGER"
      , literalTokenStorageType = "long" })
  , (BNFC.CF.catString, LiteralTokenInfo
      { literalTokenName = "STRING"
      , literalTokenStorageType = "std::string" })
  , (BNFC.CF.catDouble, LiteralTokenInfo
      { literalTokenName = "DOUBLE"
      , literalTokenStorageType = "double" })
  ]

lookupLiteralTokenInfo :: String -> LiteralTokenInfo
lookupLiteralTokenInfo catname =
  case catname `Data.Map.lookup` literalTokenInfo of
    Nothing -> error $ "Unsupported literal token: " ++ catname
    Just res -> res

tokenDefs :: Data.Map.Map String String -> BNFC.CF.CF -> Doc
tokenDefs implicit cf = linesToText
  [ "%token " ++ tkname | tkname <- Data.Map.elems implicit ]
  $+$ linesToText (map lit2token $ BNFC.CF.cfgLiterals cf)
  $+$ linesToText (map (tokenDef "std::string")
    ["CUSTOM_" ++ BNFC.CF.wpThing name
    | BNFC.CF.TokenReg name _ _ <- cfgPragmas cf])
  where
    tokenDef storageType name = concat ["%token <", storageType, "> " , name]
    lit2token catname = let info = lookupLiteralTokenInfo catname in
      tokenDef (literalTokenStorageType info) (literalTokenName info)

bisonBraces :: String -> Doc -> Doc
bisonBraces s d = text (s ++ " {") $+$ nest 4 d $+$ "}"

codeRequires :: BisonUtils -> [BNFC.CF.Cat] -> Doc
codeRequires utils entrypts = bisonBraces "%code requires" $ linesToText
  [ "#include <string_view>"
  , "#include <optional>"
  , "#include <variant>"
  , "#include \"Absyn.hpp\""
  ] $++$ text "using yyscan_t = void*;"
  $+$ namespaceWrap utils
    (text ("using ParseResultVariant = std::variant<"
      ++ intercalate ", "
      (map ((namespacePrefix utils++) . catNameNoCoerc) entrypts)
      ++ ">;"))

nonterms :: GroupedRules -> Doc
nonterms rules = linesToText $ map nonterm $ Data.Map.keys rules
  where
    nonterm cat = concat
      [ "%nterm <"
      , catNameNoCoerc cat
      , "> "
      , catNameWithCoerc cat
      ]

codeProvides :: BisonUtils -> [BNFC.CF.Cat] -> Doc
codeProvides utils entrypoints = bisonBraces "%code provides"
  $ namespaceWrap utils $ maybeImportParserClass $+$ linesToText
  [ "using ParseResultOrError ="
  , "    std::variant<ParseResultVariant, Parser::syntax_error>;"
  , "ParseResultOrError Parse(FILE* file);"
  , "ParseResultOrError Parse(std::string_view str);"
  ] $+$ foldr ($+$) empty (map entrypoint entrypoints)
  where
    -- | Bison generates the parser class in the `yy` namespace when no custom
    -- package name is provided. If we want the BNFC-generated parser to be
    -- contained in the global namespace, we have to add this line:
    maybeImportParserClass = if inPackage utils then empty else
      text "using yy::Parser;"
    entrypoint cat = linesToText
      [ funcName ++ "(FILE* file);"
      , funcName ++ "(std::string_view str);"
      ]
      where
        classname = catNameNoCoerc cat
        funcName = concat
          [ "std::variant<"
          , namespacePrefix utils
          , classname
          , ", Parser::syntax_error> Parse"
          , classname
          ]

codeLex :: BisonUtils -> Doc
codeLex utils = bisonBraces "%code" $
  text (concat ["extern ", namespacePrefix utils, "Parser::symbol_type "
         , namespaceNameOrEmpty utils, "lex(yyscan_t scanner);"])
  $++$ namespaceWrap utils (linesToText
    [ "inline Parser::symbol_type yylex(yyscan_t scanner) {"
    , "    return " ++ namespaceNameOrEmpty utils ++ "lex(scanner);"
    , "}"
    ])

bisonRules :: [String] -> Doc
bisonRules ls = nest 4 $
  (case ls of
  [] -> empty
  first:tail -> text (": " ++ first) $+$ linesToText (map ("| "++) tail)
  ) $+$ text ";"

startRules :: [BNFC.CF.Cat] -> Doc
startRules entrypoints =
  text "__start__" $+$ bisonRules (map rule entrypoints)
  where
    rule cat = sentFormCatToBisonName cat
      ++ " YYEOF { *result = {{ParseResultVariant(std::move($1))}}; }"

category :: Data.Map.Map String String -> BNFC.CF.Cat -> [BNFC.CF.Rule] -> Doc
category implicitTokenNames cat rules = case cat of
  BNFC.CF.ListCat _ -> text (catNameWithCoerc cat) $+$ bisonRules
      [makeRule r | r <- rules, BNFC.CF.internal r == BNFC.CF.Parsable]
    where
      makeRule r = let rhs = BNFC.CF.rhsRule r in
        case BNFC.CF.funName r of
        "_" -> coercionRule rhs
        "(:)" -> concat
          [ "/* (:) */ "
          , sentFormToBison rhs
          , " { $$ = std::move($"
          , show dollarList
          , "); $$.push_front(std::move($"
          , show dollarItem
          , ")); }"
          ]
          where
            [dollarItem, dollarList] = rhsObjectIndices rhs
        "(:[])" -> concat
          [ "/* (:[]) */ "
          , sentFormToBison rhs
          , " { $$.push_front(std::move($"
          , show dollarItem
          , ")); }"
          ]
          where
            [dollarItem] = rhsObjectIndices rhs
        "[]" -> "/* [] */ { }"
        name -> error ("Invalid name for a list category: " ++ name)
    
  _ -> text (catNameWithCoerc cat) $+$ bisonRules
      [makeRule r | r <- rules, BNFC.CF.internal r == BNFC.CF.Parsable]
    where
      makeRule r = case BNFC.CF.funName r of
        "_" -> coercionRule (BNFC.CF.rhsRule r)
        name -> emplacementRule name (BNFC.CF.rhsRule r)
  where
    coercionRule :: BNFC.CF.SentForm -> String
    coercionRule rhs = concat
        [ "/* _ */ "
        , sentFormToBison rhs
        , " { $$ = std::move($"
        , show $ case rhsObjectIndices rhs of
          [i] -> i
          _ -> error "Coercion object count /= 1"
        , "); }"
        ]
    emplacementRule name rhs = concat
      [ "/* "
      , name
      , " */ "
      , sentFormToBison rhs
      , " { $$.emplace<"
      , name
      , ">("
      , intercalate ", " ["std::move($" ++ show i ++ ")"
        | i <- rhsObjectIndices rhs]
      , "); }"
      ]
    sentFormToBison :: BNFC.CF.SentForm -> String
    sentFormToBison = unwords . map (\case
      Left cat -> sentFormCatToBisonName cat
      Right s -> (case s `Data.Map.lookup` implicitTokenNames of
        Nothing -> error "string token not named"
        Just name -> name))
    rhsObjectIndices :: BNFC.CF.SentForm -> [Int]
    rhsObjectIndices rhs = [i | (Left _, i) <- zip rhs [1..]]

sentFormCatToBisonName :: BNFC.CF.Cat -> String
sentFormCatToBisonName = \case
  BNFC.CF.TokenCat tokenName ->
    case Data.Map.lookup tokenName literalTokenInfo of
      Nothing -> "CUSTOM_" ++ tokenName
      Just info -> literalTokenName info
  cat -> catNameWithCoerc cat

codeSection :: BisonUtils -> BNFC.Options.SharedOptions -> [BNFC.CF.Cat] -> Doc
codeSection utils opts entrypoints =
  text "#include \"PatternMatching.hpp\""
  $++$ namespaceWrap utils
    (scannerDecl opts
    $++$ linesToText
  [ "void Parser::error(const std::string& msg) {"
  , "    *result = {{syntax_error(msg)}};"
  , "}"
  , ""
  , "static ParseResultOrError Parse(const " ++ scannerName
    ++ "& scanner) {"
  , "    std::optional<ParseResultOrError> res;"
  , "    Parser parser(scanner.FlexScanner(), &res);"
  , "    parser.parse();"
  , "    if (!res) return {Parser::syntax_error(\"\")};"
  , "    return *res;"
  , "}"
  , ""
  , "ParseResultOrError Parse(FILE* file) {"
  , "    return Parse(" ++ scannerName ++ "(file));"
  , "}"
  , ""
  , "ParseResultOrError Parse(std::string_view str) {"
  , "    return Parse(" ++ scannerName ++ "(str));"
  , "}"
  , ""
  , "template <class T>"
  , "static std::variant<T, Parser::syntax_error>"
  , "EnsureParsedType(ParseResultOrError&& parsed) {"
  , "    using RetType = std::variant<T, Parser::syntax_error>;"
  , "    return std::move(parsed) | PatternMatch{"
  , "        [](Parser::syntax_error&& err) -> RetType {"
  , "            return std::move(err);"
  , "        },"
  , "        [](ParseResultVariant&& var) -> RetType {"
  , "            return std::move(var) | PatternMatch{"
  , "                [](T&& target) -> RetType { return std::move(target); },"
  , "                [](auto&& node) -> RetType {"
  , "                    using gotType = std::decay_t<decltype(node)>;"
  , "                    std::string msg = " ++
                "\"Unexpected syntax: tried to parse \";"
  , "                    msg.append(reflection::SyntaxNodeName<T>)"
  , "                        .append(\", but got \")"
  , "                        .append(reflection::SyntaxNodeName<gotType>);"
  , "                    return Parser::syntax_error(msg);"
  , "                }"
  , "            };"
  , "        }"
  , "    };"
  , "}"
  ] $++$ vcatSpaced (map (entrypointImpl . catNameNoCoerc) entrypoints)
    )
  where
    scannerName = case BNFC.Options.inPackage opts of
      Nothing -> "Scanner"
      Just ns -> ns ++ "Scanner"
    entrypointImpl name = linesToText
      [ "std::variant<" ++ name ++
        ", Parser::syntax_error> Parse" ++ name ++ "(FILE* file) {"
      , "    return EnsureParsedType<" ++ name ++ ">(Parse(file));"
      , "}"
      , ""
      , "std::variant<" ++ name ++ ", Parser::syntax_error> Parse"
        ++ name ++ "(std::string_view str) {"
      , "    return EnsureParsedType<" ++ name ++ ">(Parse(str));"
      , "}"
      ]
