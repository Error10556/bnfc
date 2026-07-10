{-|
  Module      : BNFC.Backend.CPPVar.BisonGen
  Description : Bison grammar generator.

  Bison grammar generator.
-}

module BNFC.Backend.CPPVar.BisonGen
  (
    -- * The entrypoint
    makeBison

    -- * File naming
  , bisonFilename
  ) where

-- Language imports
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

import Text.PrettyPrint

-- BNFC imports
import qualified BNFC.Options as Options
import qualified BNFC.CF as CF

import BNFC.Backend.CPPVar.CPPUtil
import qualified BNFC.Backend.CPPVar.FlexGen as FlexGen
import qualified BNFC.Backend.CPPVar.AbsynGen as AbsynGen

-- | Returns the name of the Bison grammar file.
bisonFilename ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> String
bisonFilename opts = Options.lang opts ++ ".ypp"

-- | Generates the Bison grammar file.
makeBison ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> FlexGen.NamedImplicitTokens
    -- ^ What the terminals specified as literal strings are named.
  -> [CF.Literal]           -- ^ All used built-in tokens.
  -> [CF.Pragma]            -- ^ Grammar pragmas (contain user-defined tokens).
  -> GroupedRules           -- ^ Rules grouped by the category.
  -> AbsynGen.ListItemStorage  -- ^ How to access list elements.
  -> Doc
makeBison opts implicitTokenNames literals pragmas
    groupedRules@(GroupedRules rulemap) storeListItemsBy =
  bisonHeader opts
  $++$ tokenDefs implicitTokenNames literals pragmas
  $++$ codeRequires utils entrypoints
  $++$ nonterms groupedRules
  $++$ codeProvides utils entrypoints
  $++$ codeLex utils
  $++$ text "%start __start__"
  $++$ text "%%"
  $++$ startRules entrypoints
  $++$ vcatSpaced (map (uncurry $ category implicitTokenNames storeListItemsBy)
      $ Map.toList rulemap)
  $++$ text "%%"
  $++$ codeSection utils opts entrypoints
  where
    utils       = newBisonUtils opts
    entrypoints = extractEntrypoints pragmas groupedRules

------------------------------------------------------------------------
-- * General utility.
------------------------------------------------------------------------

-- | A collection of commonly used functions that all depend on the options.
data BisonUtils = BisonUtils
  { inPackage            :: !Bool
    -- ^ Has the package name been specified?
  , namespaceWrap        :: !(Doc -> Doc)
    -- ^ Wraps a document in a @namespace@ with the package name, but only if
    -- one has been specified.
  , namespaceNameOrEmpty :: !String
    -- ^ The specified package name, if any.
  , namespacePrefix      :: !String
    -- ^ If a package name has been given, equals "package_name::".
    -- Otherwise, the empty string.
  }

-- | Constructs a t'BisonUtils' record respecting the
-- 'BNFC.Options.inPackage' value.
newBisonUtils :: Options.SharedOptions -> BisonUtils
newBisonUtils opts = case Options.inPackage opts of
  Nothing -> BisonUtils
    { inPackage            = False
    , namespaceWrap        = id
    , namespaceNameOrEmpty = ""
    , namespacePrefix      = ""
    }
  Just ns -> BisonUtils
    { inPackage            = True
    , namespaceWrap        = wrapNamespace ns
    , namespaceNameOrEmpty = ns
    , namespacePrefix      = ns ++ "::"
    }

-- | Information about a literal token
-- (String, Ident, Integer, Double, or Char).
data LiteralTokenInfo = LiteralTokenInfo
  { literalTokenName        :: String
    -- ^ How the token is named in the Bison grammar (i.e. the upper-case name).
  , literalTokenStorageType :: String
    -- ^ The C++ type of the value that the token holds
    -- (e.g. @std::string@ for the String token).
  }

-- | Maps the token category name to its properties relevant to Bison.
literalTokenInfo :: Map String LiteralTokenInfo
literalTokenInfo = Map.fromList
  [ (CF.catIdent, LiteralTokenInfo
      { literalTokenName        = "IDENT"
      , literalTokenStorageType = "std::string"
      })
  , (CF.catChar, LiteralTokenInfo
      { literalTokenName        = "CHAR"
      , literalTokenStorageType = "int32_t"
      })
  , (CF.catInteger, LiteralTokenInfo
      { literalTokenName        = "INTEGER"
      , literalTokenStorageType = "long"
      })
  , (CF.catString, LiteralTokenInfo
      { literalTokenName        = "STRING"
      , literalTokenStorageType = "std::string"
      })
  , (CF.catDouble, LiteralTokenInfo
      { literalTokenName        = "DOUBLE"
      , literalTokenStorageType = "double"
      })
  ]

-- | Finds the token information by its name. Throws if given an unknown name.
lookupLiteralTokenInfo :: String -> LiteralTokenInfo
lookupLiteralTokenInfo catname =
  case catname `Map.lookup` literalTokenInfo of
    Nothing  -> error $ "Unsupported literal token: " ++ catname
    Just res -> res

-- | Wraps a 'Doc' in curly braces, indenting it by 4 spaces.
-- Puts an arbitrary string as the heading.
bisonBraces ::
     String  -- ^ The heading.
  -> Doc     -- ^ Block to wrap and indent.
  -> Doc
bisonBraces s d = text (s ++ " {") $+$ nest 4 d $+$ text "}"

------------------------------------------------------------------------
-- * Bison top section.
------------------------------------------------------------------------

-- | Generates directives at the top of the file.
bisonHeader ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> Doc
bisonHeader opts = linesToText
  [ "%require \"3.2\""
  , "%language \"c++\""
  , "%define api.value.type variant"
  , "%define api.token.constructor"
  , "%define api.parser.class {Parser}"
  ]
  $+$ case Options.inPackage opts of
    Nothing -> empty
    Just ns -> text ("%define api.namespace {" ++ ns ++ "}")
  $+$ linesToText
  [ "%lex-param {yyscan_t scanner}"
  , "%parse-param {yyscan_t scanner}"
  , "%parse-param {std::optional<std::variant<ParseResultVariant, "
    ++ "syntax_error>>* result}"
  , "%header \"" ++ Options.lang opts ++ ".tab.hpp\""
  ]

-- | Generates the token definitions.
tokenDefs ::
     FlexGen.NamedImplicitTokens
    -- ^ What the terminals specified as literal strings are named.
  -> [CF.Literal]  -- ^ All used built-in tokens.
  -> [CF.Pragma]   -- ^ Grammar pragmas (contain user-defined tokens).
  -> Doc
tokenDefs (FlexGen.NamedImplicitTokens implicit) literals pragmas = linesToText
  ["%token " ++ tkname | tkname <- Map.elems implicit]
  $+$ linesToText (map lit2token literals)
  $+$ linesToText (map (tokenDef "std::string")
    ["CUSTOM_" ++ CF.wpThing name
    | CF.TokenReg name _ _ <- pragmas])
  where
    tokenDef storageType name = concat ["%token <", storageType, "> " , name]
    lit2token catname =
      let info = lookupLiteralTokenInfo catname
      in tokenDef (literalTokenStorageType info) (literalTokenName info)

-- | Generates code that includes necessary definitions for the parser.
codeRequires ::
     BisonUtils
  -> [CF.Cat]  -- ^ Grammar entrypoints.
  -> Doc
codeRequires utils entrypts = bisonBraces "%code requires" $ linesToText
  [ "#include <string_view>"
  , "#include <optional>"
  , "#include <variant>"
  , "#include \"Absyn.hpp\""
  ] $++$ text "using yyscan_t = void*;"
  $++$ namespaceWrap utils
    (text ("using ParseResultVariant = std::variant<"
      ++ intercalate ", "
        [namespacePrefix utils ++ catNameNoCoerc cat | cat <- entrypts]
      ++ ">;"))

-- | Generates declarations of nonterminals (BNFC categories).
nonterms :: GroupedRules -> Doc
nonterms (GroupedRules rulemap) = linesToText $ map nonterm $ Map.keys rulemap
  where
    nonterm cat = concat
      [ "%nterm <"
      , nontokenCatNameNoCoerc cat
      , "> "
      , nontokenCatNameWithCoerc cat
      ]

-- | Generates the public declarations used by the client.
codeProvides ::
     BisonUtils
  -> [CF.Cat]  -- ^ Grammar entrypoints.
  -> Doc
codeProvides utils entrypoints = bisonBraces "%code provides"
  $ namespaceWrap utils
    $ maybeImportParserClass $+$ linesToText
    [ "using ParseResultOrError ="
    , "    std::variant<ParseResultVariant, Parser::syntax_error>;"
    , "ParseResultOrError Parse(FILE* file);"
    , "ParseResultOrError Parse(std::string_view str);"
    ] $+$ foldr ($+$) empty (map entrypoint entrypoints)
  where
    -- | Bison generates the parser class in the `yy` namespace when no custom
    -- package name is provided. If we want the BNFC-generated parser to be
    -- contained in the global namespace, we have to import it explicitly.
    maybeImportParserClass
      | inPackage utils = empty
      | otherwise       = text "using yy::Parser;"
    entrypoint cat = linesToText
      [ funcName ++ "(FILE* file);"
      , funcName ++ "(std::string_view str);"
      ]
      where
        funcName = concat
          [ "std::variant<"
          , namespacePrefix utils
          , catNameNoCoerc cat
          , ", Parser::syntax_error> Parse"
          , catNameNoCoerc cat
          ]

-- | Generates a bit of code that makes the lexer available.
codeLex :: BisonUtils -> Doc
codeLex utils = bisonBraces "%code"
  $ text (concat
    ["extern "
    , namespacePrefix utils
    , "Parser::symbol_type "
    , namespaceNameOrEmpty utils
    , "lex(yyscan_t scanner);"
    ])
  $++$ namespaceWrap utils (linesToText
    [ "static inline Parser::symbol_type yylex(yyscan_t scanner) {"
    , "    return " ++ namespaceNameOrEmpty utils ++ "lex(scanner);"
    , "}"
    ])

------------------------------------------------------------------------
-- * Utility for the Bison rule section.
------------------------------------------------------------------------

-- | Formats the alternative bison rules.
bisonRules ::
     [String]  -- ^ A list of rules.
  -> Doc
bisonRules ls = nest 4
  $ (case ls of
      []           -> empty
      first : tail -> text (": " ++ first) $+$ linesToText (map ("| " ++) tail)
    ) $+$ text ";"

-- | Converts a single BNFC sentence form nonterminal to its Bison rule name.
sentFormCatToBisonName :: CF.Cat -> String
sentFormCatToBisonName = \case
  CF.TokenCat tokenName ->
    case Map.lookup tokenName literalTokenInfo of
      Nothing   -> "CUSTOM_" ++ tokenName
      Just info -> literalTokenName info
  cat                   -> catNameWithCoerc cat

------------------------------------------------------------------------
-- * Bison rule section.
------------------------------------------------------------------------

-- | Generates the entrypoint alternatives as grammar rules.
startRules ::
     [CF.Cat]  -- ^ Grammar entrypoints.
  -> Doc
startRules entrypoints =
  text "__start__" $+$ bisonRules (map rule entrypoints)
  where
    rule cat = sentFormCatToBisonName cat
      ++ " YYEOF { *result = {{ParseResultVariant(std::move($1))}}; }"

-- | Generates all Bison rules for a category.
category ::
     FlexGen.NamedImplicitTokens
    -- ^ What the terminals specified as literal strings are named.
  -> AbsynGen.ListItemStorage
  -> NontokenCategory  -- ^ The nonterminal.
  -> [CF.Rule]         -- ^ The rules that produce the nonterminal.
  -> Doc
category (FlexGen.NamedImplicitTokens implicitTokenNames)
    storeListItemsBy cat rules =
  case cat of
    Nontoken_ListCat lElem -> text (nontokenCatNameWithCoerc cat) $+$ bisonRules
        [makeRule r | r <- rules, CF.internal r == CF.Parsable]
      where
        lElemClass = catNameNoCoerc lElem
        maybeMakeUnique = case storeListItemsBy of
          AbsynGen.StoreByValue   -> id
          AbsynGen.StoreByPointer -> \ s -> concat
            ["std::make_unique<" , lElemClass , ">(" , s , ")"]
        makeRule r =
          let rhs = CF.rhsRule r
          in case CF.funName r of
            "_"     -> coercionRule rhs
            "(:)"   -> concat
              [ "/* (:) */ "
              , sentFormToBison rhs
              , " { $$ = std::move($"
              , show dollarList
              , "); $$.push_front("
              , maybeMakeUnique $ concat
                [ "std::move($"
                , show dollarItem
                , ")"
                ]
              , "); }"
              ]
              where
                [dollarItem, dollarList] = rhsObjectIndices rhs
            "(:[])" -> concat
              [ "/* (:[]) */ "
              , sentFormToBison rhs
              , " { $$.push_front("
              , maybeMakeUnique $ concat
                [ "std::move($"
                , show dollarItem
                , ")"
                ]
              , "); }"
              ]
              where
                [dollarItem] = rhsObjectIndices rhs
            "[]"    -> "/* [] */ " ++ sentFormToBison rhs ++ " { }"
            name    -> error ("Invalid name for a list category: " ++ name)

    -- non-list
    _ -> text (nontokenCatNameWithCoerc cat) $+$ bisonRules
        [makeRule r | r <- rules, CF.internal r == CF.Parsable]
      where
        makeRule r = case CF.funName r of
          "_"  -> coercionRule (CF.rhsRule r)
          name ->
            if isClassLabel name
            then emplacementRule name (CF.rhsRule r)
            else functionRule    name (CF.rhsRule r)
  where
    coercionRule :: CF.SentForm -> String
    coercionRule rhs = concat
        [ "/* _ */ "
        , sentFormToBison rhs
        , " { $$ = std::move($"
        , show $ case rhsObjectIndices rhs of
          [i] -> i
          _   -> error "Coercion object count /= 1"
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
      , makeArgs rhs
      , "); }"
      ]
    functionRule name rhs = concat
      [ "/* "
      , name
      , " */ "
      , sentFormToBison rhs
      , " { $$ = make_"
      , name
      , "("
      , makeArgs rhs
      , "); }"
      ]
    sentFormToBison :: CF.SentForm -> String
    sentFormToBison = unwords . unempty . map (\case
      Left cat -> sentFormCatToBisonName cat
      Right s  -> (case s `Map.lookup` implicitTokenNames of
        Nothing   -> error "string token not named"
        Just name -> name))
      where
        unempty = \case
          []       -> ["/* empty */"]
          nonempty -> nonempty
    rhsObjectIndices :: CF.SentForm -> [Int]
    rhsObjectIndices rhs = [i | (Left _, i) <- zip rhs [1..]]
    makeArgs :: CF.SentForm -> String
    makeArgs rhs = intercalate ", "
      [ "std::move($" ++ show i ++ ")" | i <- rhsObjectIndices rhs]

------------------------------------------------------------------------
-- * Implementations.
------------------------------------------------------------------------

-- | Generates the implementations for the parsing methods.
codeSection ::
     BisonUtils
  -> Options.SharedOptions  -- ^ BNFC invokation options.
  -> [CF.Cat]               -- ^ Grammar entrypoints.
  -> Doc
codeSection utils opts entrypoints =
  text "#include \"PatternMatching.hpp\""
  $++$ namespaceWrap utils
    (FlexGen.scannerDecl opts
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
  , "    return std::move(*res);"
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
  , "                    std::string msg = "
    ++ "\"Unexpected syntax: tried to parse \";"
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
    scannerName = case Options.inPackage opts of
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
