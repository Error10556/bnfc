{-|
  Module      : BNFC.Backend.CPPVar.AbsynGen
  Description : Abstract syntax node classes generator.

  Abstract syntax node classes generator.
-}

module BNFC.Backend.CPPVar.AbsynGen
  (
    -- * The entrypoint
    makeAbsyn
  , GeneratedAbsyn(..)

    -- * File naming
  , absynHppFilename
  , absynCppFilename

    -- * Utility
  , tokenStorageName
  ) where

-- Language imports
import Prelude hiding ((<>))
import qualified Data.Map as Map
import Data.List (intercalate, partition)

import Text.PrettyPrint (Doc, text, ($+$), empty, nest, (<>))

-- BNFC imports
import qualified BNFC.CF as CF
import qualified BNFC.Options as Options
import BNFC.Backend.CPPVar.CPPUtil

-- | The name of the header file defining abstract syntax classes.
absynHppFilename :: String
absynHppFilename = "Absyn.hpp"

-- | The name of the source file implementing abstract syntax classes.
absynCppFilename :: String
absynCppFilename = "Absyn.cpp"

-- | Generates the abstract syntax node classes.
makeAbsyn ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> [CF.Literal]           -- ^ Used built-in tokens.
  -> [CF.Pragma]            -- ^ User-defined pragmas (contain custom tokens).
  -> MergedGroupedRules
    -- ^ Rule labels in the grammar description,
    -- grouped by the grammar category.
  -> GeneratedAbsyn
makeAbsyn opts literals pragmas mergedGroupedRules = GeneratedAbsyn
  { absynCode = CPPHeaderSourcePair
    { cppHeaderText = hpp
    , cppSourceText = cpp
    }
  , absynListItemsByPointer = {- undefined -} False
  }
  where
    maybeNamespace = wrapPackage opts
    StructWithReflection
      { structWithReflection_struct = hppTokenStructs
      , structWithReflection_reflection = hppTokenRefl
      } = headerTokens literals pragmas
    StructWithReflection
      { structWithReflection_struct = hppCatDefs
      , structWithReflection_reflection = hppCatRefl
      } = headerCats mergedGroupedRules
    RuleCode
      { ruleCode_declaration = hppRules
      , ruleCode_reflection = hppRuleRefl
      , ruleCode_implementation = cppRules
      } = rules mergedGroupedRules
    hppMain = hppTokenStructs $++$ hppCatDefs $++$ hppRules
    hppRefl = reflectionTemplates $++$ hppTokenRefl $++$ hppCatRefl
      $++$ hppRuleRefl
    hpp = headerHead $++$ maybeNamespace
      (hppMain $++$ wrapNamespace "reflection" hppRefl)
    cpp = text ("#include \"" ++ absynHppFilename ++ "\"")
      $++$ maybeNamespace
        (clonePtrImpl $++$ implTokens literals pragmas $++$ cppRules)

-- | Code and the decision about list item storage. Returned from 'makeAbsyn'.
data GeneratedAbsyn = GeneratedAbsyn
  { absynCode               :: !CPPHeaderSourcePair  -- ^ Generated code.
  , absynListItemsByPointer :: !Bool
    -- ^ @True@ if we generated list classes storing pointers,
    -- @False@ if storing values directly.
  }

------------------------------------------------------------------------
-- * Handle type completeness (with reordering and pointers).
------------------------------------------------------------------------

-- -- | Representation of a to-be-generated class declaration.
-- -- Used in a list to specify the order of declarations.
-- data ClassDeclaration
--   = ListClassDeclaration   !CF.Cat
--     -- ^ A list class (BNFC list category).
--     -- The stored t'CF.Cat' is the list element.
--   | NormalClassDeclaration !CF.Rule  -- ^ A normal class (BNFC label).
--
-- getUnorderedClassDeclarations ::
--      MergedGroupedRules
--   -> [ClassDeclaration]
-- getUnorderedClassDeclarations (GroupedRules rulemap) =
--   flip concatMap (Map.toList rulemap) $ \case
--     (CF.ListCat elemCat, _) -> ListClassDeclaration elemCat
--     (CF.
--
-- decideClassDeclarations ::
--      CF                           -- ^ Grammar description.
--   -> Options.ListItemStorageType  -- ^ How to store items.
--   -> [ClassDeclaration]
-- decideClassDeclarations cf itemType = undefined
--
-- tryReorderClasses :: GroupedRules -> [ClassDeclaration]
-- tryReorderClasses cf = undefined
--   where
--     allDecls = getUnorderedClassDeclarations cf
--     nDecls   = length allDecls
--     classDeclaration2index :: Map ClassDeclaration Int
--     classDeclaration2index = Map.fromList $ zip allDecls [0..]
--

------------------------------------------------------------------------
-- * Boilerplate.
------------------------------------------------------------------------

-- | @include@ directives at the top.
headerHead :: Doc
headerHead = linesToText
  [ "#pragma once"
  , "#include <memory>"
  , "#include <string>"
  , "#include <deque>"
  , "#include <variant>"
  ]

-- | Declaration of templates in the @reflection@ namespace.
reflectionTemplates :: Doc
reflectionTemplates = linesToText
  [ "template<class T> struct CoercionLevel_t {};"
  , "template<class T> constexpr int CoercionLevel = CoercionLevel_t<T>::value;"
  , ""
  , "template<class T> struct SyntaxNodeName_t {};"
  , "template<class T>"
  , "constexpr const char* SyntaxNodeName = SyntaxNodeName_t<T>::value;"
  ]

-- | The text of a @CoercionLevel_t@ specialization.
rawCoercionSpec :: String -> Integer -> Doc
rawCoercionSpec name coercion = linesToText
  [ "template<> struct CoercionLevel_t<" ++ name ++ ">"
  , "{ static constexpr int value = " ++ show coercion ++ "; };"
  ]

-- | The text of a @SyntaxNodeName_t@ specialization.
rawNodeNameSpec :: String -> Doc
rawNodeNameSpec name = linesToText
  [ "template<> struct SyntaxNodeName_t<" ++ name ++ ">"
  , "{ static constexpr const char* value = \"" ++ name ++ "\"; };"
  ]

-- | Static utility function: clones the contents of a @unique_ptr@,
-- putting the clone into another @unique_ptr@.
clonePtrImpl :: Doc
clonePtrImpl = linesToText
  [ "template <class T>"
  , "static std::unique_ptr<T> ClonePtr(const std::unique_ptr<T>& p) {"
  , "    if (!p) return {};"
  , "    return std::make_unique<T>(*p);"
  , "}"
  ]

------------------------------------------------------------------------
-- * Tokens.
------------------------------------------------------------------------

-- | Returns an appropriate name for a token field storing its value.
tokenStorageName ::
     String  -- ^ The name of the token class.
  -> String  -- ^ The name of the storage field.
tokenStorageName "Value" = "MyValue"
tokenStorageName _ = "Value"

-- | A pair of a @struct@ and its property definitions.
-- Used internally to return both from one function.
data StructWithReflection = StructWithReflection
  { structWithReflection_struct     :: !Doc  -- ^ The @struct@.
  , structWithReflection_reflection :: !Doc  -- ^ The property definitions.
  }

-- | Turns a list of t'StructWithReflection' into
-- a list of structs with a list of reflections.
unzipStructWithReflection ::
     [StructWithReflection]
  -> ([Doc], [Doc])
unzipStructWithReflection = foldr (\ StructWithReflection
    { structWithReflection_struct = struct
    , structWithReflection_reflection = refl
    } (ss, refls) -> (struct : ss, refl : refls)
  ) ([], [])

-- | Generates a declaration for a token structure with constructors and
-- assignment operators taking references to the storage type.
tokenStructWithRefConstructorsHeader ::
     String  -- ^ The token name.
  -> String  -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> StructWithReflection
tokenStructWithRefConstructorsHeader name storageType = StructWithReflection
  { structWithReflection_struct = linesToText
    [ "struct " ++ name ++ " {"
    , "public:"
    ] $+$ nest 4 (linesToText
      [ storageType ++ " " ++ tokenStorageName name ++ ";"
      , name ++ "() = default;"
      , name ++ "(const " ++ name ++ "&) = default;"
      , name ++ "(" ++ name ++ "&&) = default;"
      , name ++ "& operator=(const " ++ name ++ "&) = default;"
      , name ++ "& operator=(" ++ name ++ "&&) = default;"
      , name ++ "(const " ++ storageType ++ "&); /* implicit */"
      , name ++ "(" ++ storageType ++ "&&); /* implicit */"
      , name ++ "& operator=(const " ++ storageType ++ "&);"
      , name ++ "& operator=(" ++ storageType ++ "&&);"
      ]) $+$ text "};"
  , structWithReflection_reflection =
    rawCoercionSpec name 0 $+$ rawNodeNameSpec name
  }

-- | Generates an implementation for a token structure with constructors and
-- assignment operators taking references to the storage type.
tokenStructWithRefConstructorsImpl ::
     String  -- ^ The token name.
  -> String  -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> Doc
tokenStructWithRefConstructorsImpl name storageType =
  linesToText
    [ "// token: " ++ name
    , ""
    , name ++ "::" ++ name ++ "(const " ++ storageType ++ "& v)"
    , "    : " ++ tokenStorageName name ++ "(v) {}"
    , ""
    , name ++ "::" ++ name ++ "(" ++ storageType ++ "&& v)"
    , "    : " ++ tokenStorageName name ++ "(std::move(v)) {}"
    , ""
    , name ++ "& " ++ name ++ "::operator=(const " ++ storageType ++ "& v) {"
    , "    " ++ tokenStorageName name ++ " = v;"
    , "    return *this;"
    , "}"
    , ""
    , name ++ "& " ++ name ++ "::operator=(" ++ storageType ++ "&& v) {"
    , "    " ++ tokenStorageName name ++ " = std::move(v);"
    , "    return *this;"
    , "}"
    ]

-- | Generates a declaration for a token structure with a by-value constructor
-- and assignment operator.
tokenStructHeader ::
     String  -- ^ The token name.
  -> String  -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> StructWithReflection
tokenStructHeader name storageType = StructWithReflection
  { structWithReflection_struct = linesToText
    [ "struct " ++ name ++ " {"
    , "public:"
    ] $+$ nest 4 (linesToText
      [ storageType ++ " " ++ tokenStorageName name ++ ";"
      , name ++ "() = default;"
      , name ++ "(const " ++ name ++ "&) = default;"
      , name ++ "& operator=(const " ++ name ++ "&) = default;"
      , name ++ "& operator=(" ++ name ++ "&&) = default;"
      , name ++ "(" ++ storageType ++ "); /* implicit */"
      , name ++ "& operator=(" ++ storageType ++ ");"
      ]) $+$ text "};"
  , structWithReflection_reflection =
    rawCoercionSpec name 0 $+$ rawNodeNameSpec name
  }

-- | Generates an implementation for a token structure with a by-value
-- constructor and assignment operator.
tokenStructImpl ::
     String  -- ^ The token name.
  -> String  -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> Doc
tokenStructImpl name storageType =
  linesToText
    [ "// token: " ++ name
    , ""
    , name ++ "::" ++ name ++ "(" ++ storageType ++ " v)"
    , "    : " ++ tokenStorageName name ++ "(v) {}"
    , ""
    , name ++ "& " ++ name ++ "::operator=(" ++ storageType ++ " v) {"
    , "    " ++ tokenStorageName name ++ " = v;"
    , "    return *this;"
    , "}"
    ]

-- | Generates declarations for all tokens.
headerTokens ::
     [CF.Literal]  -- ^ The built-in tokens.
  -> [CF.Pragma]   -- ^ Contains user-defined tokens.
  -> StructWithReflection
headerTokens lits pragmas = StructWithReflection
  { structWithReflection_struct     = vcatSpaced structs
  , structWithReflection_reflection = vcatSpaced reflections
  }
  where
    litTokens  = map makeLitToken lits
    userTokens =
      [makeUserToken $ CF.wpThing name | CF.TokenReg name _ _ <- pragmas]
    makeLitToken s
      | s == "Char"    = tokenStructHeader s "int32_t"
      | s == "String"  = tokenStructWithRefConstructorsHeader s "std::string"
      | s == "Integer" = tokenStructHeader s "long"
      | s == "Double"  = tokenStructHeader s "double"
      | otherwise {- "Ident" -} = makeUserToken s
    makeUserToken s = tokenStructWithRefConstructorsHeader s "std::string"
    (structs, reflections) = unzipStructWithReflection (litTokens ++ userTokens)

-- | Generates implementations for all tokens.
implTokens ::
     [CF.Literal]  -- ^ The built-in tokens.
  -> [CF.Pragma]   -- ^ Contains user-defined tokens.
  -> Doc
implTokens lits pragmas = vcatSpaced $ litTokens ++ userTokens
  where
    litTokens = map makeLitToken lits
    userTokens =
      [makeUserToken $ CF.wpThing name | CF.TokenReg name _ _ <- pragmas]
    makeLitToken s
      | s == "Char"    = tokenStructImpl s "int32_t"
      | s == "String"  = tokenStructWithRefConstructorsImpl s "std::string"
      | s == "Integer" = tokenStructImpl s "long"
      | s == "Double"  = tokenStructImpl s "double"
      | otherwise {- "Ident" -} = makeUserToken s
    makeUserToken s = tokenStructWithRefConstructorsImpl s "std::string"

------------------------------------------------------------------------
-- * Categories (nonterminals).
------------------------------------------------------------------------

-- | Generates declarations for all categories.
headerCats ::
     MergedGroupedRules
  -> StructWithReflection
headerCats (MergedGroupedRules rulemap) = StructWithReflection
  { structWithReflection_struct     = vcatSpaced defs
  , structWithReflection_reflection = vcatSpaced refls
  }
  where
    -- We put all lists _after_ the normal categories (and tokens) so that
    -- the list elements are already declared when we declare the lists.
    (defs, refls) = unzipStructWithReflection
      $ map toDocument (nonlists ++ lists)
    (lists, nonlists) = partition (\ (cat, _) ->
        case cat of
          NontokenClass_Cat     _ -> False
          NontokenClass_ListCat _ -> True
      ) $ Map.toList rulemap
    toDocument :: (NontokenClassCategory, [CF.Rule]) -> StructWithReflection
    toDocument (cat, rules) =
      let name = nontokenClassCatName cat
      in case cat of
        NontokenClass_ListCat elemCat -> StructWithReflection
          { structWithReflection_struct =
              text $ "struct " ++ name ++ " : public std::deque<"
              ++ catNameNoCoerc elemCat ++ "> {};"
          , structWithReflection_reflection =
              rawCoercionSpec name 0 $+$ rawNodeNameSpec name
          }
        NontokenClass_Cat     _       ->
          let ruleNames = filter (/= "_") $ map CF.funName rules
          in StructWithReflection
            { structWithReflection_struct =
                linesToText (map (("class " ++) . (++ ";")) ruleNames)
                $+$ text ("using " ++ name ++ " = std::variant<"
                  ++ intercalate ", " ruleNames ++ ">;")
            , structWithReflection_reflection = rawNodeNameSpec name
            }

------------------------------------------------------------------------
-- * Label classes.
------------------------------------------------------------------------

-- | A record combining the declaration, property definitions, and
-- implementation of a rule label class.
data RuleCode = RuleCode
  { ruleCode_declaration    :: !Doc  -- ^ The class declaration.
  , ruleCode_reflection     :: !Doc  -- ^ The properties.
  , ruleCode_implementation :: !Doc  -- ^ The method implementations.
  }

-- | Turns a list of t'RuleCode's into
-- a list of class declarations, a list of reflection properties, and
-- a list of implementations.
unzipRuleCode :: [RuleCode] -> ([Doc], [Doc], [Doc])
unzipRuleCode = foldr (\ RuleCode
    { ruleCode_declaration    = decl
    , ruleCode_reflection     = refl
    , ruleCode_implementation = impl
    } (decls, refls, impls) -> (decl : decls, refl : refls, impl : impls)
  ) ([], [], [])

-- | Concatenates (inserting blank lines between blocks) the corresponding
-- fields of all t'RuleCode's together.
vcatRuleCode :: [RuleCode] -> RuleCode
vcatRuleCode rules = RuleCode
  { ruleCode_declaration    = vcatSpaced decls
  , ruleCode_reflection     = vcatSpaced refls
  , ruleCode_implementation = vcatSpaced impls
  }
  where
    (decls, refls, impls) = unzipRuleCode rules

-- | Generates the classes for all labels.
rules :: MergedGroupedRules -> RuleCode
rules (MergedGroupedRules rulemap) = vcatRuleCode $ map rule $ concat
  [rules | (NontokenClass_Cat _, rules) <- Map.toList rulemap]

-- | Generates the declaration, properties, and implementation for a labeled
-- BNF rule.
rule :: CF.Rule -> RuleCode
rule r = let
    name = CF.funName r
    (indexedNames, members) = unzip $ fieldNames $ CF.rhsRule r
    storageTypes = map storageType' members
    constructorSignatureOrEmpty
      | null members = empty
      | otherwise = text $ name ++ "("
        ++ intercalate ", " [catNameNoCoerc c ++ "&&" | c <- members]
        ++ ");"
    headerClass = linesToText
      [ "class " ++ name ++ " {"
      , "public:"
      ] $+$ nest 4 (linesToText
        [ name ++ "() = default;"
        , name ++ "(const " ++ name ++ "&); /* clone */"
        , name ++ "(" ++ name ++ "&&) = default;"
        , name ++ "& operator=(const " ++ name ++ "&); "
          ++ "/* discard and replace by clone */"
        , name ++ "& operator=(" ++ name ++ "&&) = default;"
        ] $+$ constructorSignatureOrEmpty -- constructor
        -- Fields below
        $+$ foldr ($+$) empty
          (map (\ (typ, name) -> text (typ ++ " " ++ name ++ ";"))
          (zip storageTypes indexedNames)))
      $+$ text "};"

    headerRefls = rawCoercionSpec name (CF.precRule r) $+$ rawNodeNameSpec name

    -- | Formats field initializers.
    ctorInitializers :: [String] -> Doc
    ctorInitializers = nest 4 . \case
      [] -> empty
      first : rest -> foldr ($+$) empty
        $ text (": " ++ first) : map (text . (", " ++)) rest

    cloneValue storageCat value
      | isPointerType' storageCat =
        "ClonePtr<" ++ catNameNoCoerc storageCat ++ ">(" ++ value ++ ")"
      | otherwise = value
    moveValue storageCat value
      | isPointerType' storageCat =
        "std::make_unique<" ++ catNameNoCoerc storageCat
          ++ ">(std::move(" ++ value ++ "))"
      | otherwise = "std::move(" ++ value ++ ")"

    copyCtor = text
      (name ++ "::" ++ name ++ "(const " ++ name ++ "& other [[maybe_unused]])")
      $+$ ctorInitializers
        [name ++ "(" ++ cloneValue cat ("other." ++ name) ++ ")"
        | (name, cat) <- zip indexedNames members] <> text " {}"
    copyAsg = text (name ++ "& " ++ name
             ++ "::operator=(const " ++ name ++ "& other [[maybe_unused]]) {")
      $+$ nest 4 (linesToText (
        [name ++ " = " ++ cloneValue cat ("other." ++ name) ++ ";"
        | (name, cat) <- zip indexedNames members]
        ++ ["return *this;"])) $+$ text "}"
    ruleCtorOrEmpty
      | null members = empty
      | otherwise = text (name ++ "::" ++ name ++ "(" ++ intercalate ", "
          [catNameNoCoerc cat ++ "&& _" ++ show i
          | (cat, i :: Int) <- zip members [1..]] ++ ")")
        $+$ ctorInitializers
          [name ++ "(" ++ moveValue cat ('_' : show i) ++ ")"
          | (name, cat, i :: Int) <- zip3 indexedNames members [1..]]
        <> text " {}"
    impl = vcatSpaced
      [ text $ "// " ++ name
      , copyCtor
      , copyAsg
      , ruleCtorOrEmpty
      ]

  in RuleCode
    { ruleCode_declaration = headerClass
    , ruleCode_reflection = headerRefls
    , ruleCode_implementation = impl
    }
  where
    storageType' :: CF.Cat -> String
    storageType' = \case
      lst@(CF.ListCat _) -> catNameNoCoerc lst
      CF.TokenCat s      -> s
      other -> "std::unique_ptr<" ++ catNameNoCoerc other ++ ">"
    isPointerType' :: CF.Cat -> Bool
    isPointerType' = \case
      CF.CoercCat _ _ -> True
      CF.Cat      _   -> True
      _               -> False
