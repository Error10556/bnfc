module BNFC.Backend.CPPVar.AbsynGen
    (makeAbsyn, absynHppFilename, absynCppFilename) where

--import BNFC.Utils
import BNFC.CF
import BNFC.Options
import Text.PrettyPrint (Doc, text, ($+$), empty, nest)
import BNFC.Backend.CPPVar.CPPUtil
import qualified Data.Map
import qualified Data.Set
import Data.List (intercalate, sort)

absynHppFilename :: String
absynHppFilename = "Absyn.hpp"

absynCppFilename :: String
absynCppFilename = "Absyn.cpp"

-- | returns (Absyn.hpp, Absyn.cpp)
makeAbsyn :: SharedOptions -> CF -> Data.Map.Map Cat [Rule] -> (Doc, Doc)
makeAbsyn opts cf groupedRules = (hpp, cpp)
    where
        maybeNamespace = maybe id wrapNamespace (inPackage opts)
        (hppTokenStructs, hppTokenRefl) = headerTokens cf
        (hppCatDefs, hppCatRefl) = headerCats groupedRules
        (hppRules, hppRuleRefl, cppRules) = rules cf
        hppMain = hppTokenStructs $++$ hppCatDefs $++$ hppRules
        hppRefl = reflectionTemplates $++$ hppTokenRefl $++$ hppCatRefl
            $++$ hppRuleRefl
        hpp = headerHead $++$ maybeNamespace
            (hppMain $++$ wrapNamespace "reflection" hppRefl)
        cpp = text ("#include \"" ++ absynHppFilename ++ "\"")
            $++$ maybeNamespace (clonePtrImpl $++$ implTokens cf $++$ cppRules)

headerHead :: Doc
headerHead = linesToText
    [ "#pragma once"
    , "#include <memory>"
    , "#include <string>"
    , "#include <deque>"
    , "#include <variant>"
    ]

reflectionTemplates :: Doc
reflectionTemplates = linesToText
    [ "template <class T> struct CoercionLevel_t {};"
    , "template<class T> constexpr int CoercionLevel = CoercionLevel_t<T>::value;"
    , ""
    , "template <class T> struct SyntaxNodeName_t {};"
    , "template<class T>"
    , "constexpr const char* SyntaxNodeName = SyntaxNodeName_t<T>::value;"
    ]

rawCoercionSpec :: String -> Integer -> Doc
rawCoercionSpec name coercion = linesToText
    [ "template<> struct CoercionLevel_t<" ++ name ++ ">"
    , "{ static constexpr int value = " ++ show coercion ++ "; };"
    ]

rawNodeNameSpec :: String -> Doc
rawNodeNameSpec name = linesToText
    [ "template<> struct SyntaxNodeName_t<" ++ name ++ ">"
    , "{ static constexpr const char* value = \"" ++ name ++ "\"; };"
    ]

tokenStorageName :: String -> String
tokenStorageName "Value" = "MyValue"
tokenStorageName _ = "Value"

-- | -> (struct, reflection)
tokenStructWithRefConstructorsHeader :: String -> String -> (Doc, Doc)
tokenStructWithRefConstructorsHeader name storageType =
    (linesToText
        [ "struct " ++ name ++ " {"
        , "public:"
        , "    " ++ storageType ++ " " ++ tokenStorageName name ++ ";"
        , "    " ++ name ++ "() = default;"
        , "    " ++ name ++ "(const " ++ name ++ "&) = default;"
        , "    " ++ name ++ "(" ++ name ++ "&&) = default;"
        , "    " ++ name ++ "& operator=(const " ++ name ++ "&) = default;"
        , "    " ++ name ++ "& operator=(" ++ name ++ "&&) = default;"
        , "    " ++ name ++ "(const " ++ storageType ++ "&); /* implicit */"
        , "    " ++ name ++ "(" ++ storageType ++ "&&); /* implicit */"
        , "    " ++ name ++ "& operator=(const " ++ storageType ++ "&);"
        , "    " ++ name ++ "& operator=(" ++ storageType ++ "&&);"
        , "};"
        ], rawCoercionSpec name 0 $+$ rawNodeNameSpec name)

tokenStructWithRefConstructorsImpl :: String -> String -> Doc
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

-- | -> (struct, reflection)
tokenStructHeader :: String -> String -> (Doc, Doc)
tokenStructHeader name storageType =
    (linesToText
        [ "struct " ++ name ++ " {"
        , "public:"
        , "    " ++ storageType ++ " " ++ tokenStorageName name ++ ";"
        , "    " ++ name ++ "() = default;"
        , "    " ++ name ++ "(const " ++ name ++ "&) = default;"
        , "    " ++ name ++ "& operator=(const " ++ name ++ "&) = default;"
        , "    " ++ name ++ "& operator=(" ++ name ++ "&&) = default;"
        , "    " ++ name ++ "(" ++ storageType ++ "); /* implicit */"
        , "    " ++ name ++ "& operator=(" ++ storageType ++ ");"
        , "};"
        ], rawCoercionSpec name 0 $+$ rawNodeNameSpec name)

tokenStructImpl :: String -> String -> Doc
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

-- | -> (structs, reflection)
headerTokens :: CF -> (Doc, Doc)
headerTokens cf = (vcatSpaced structs, vcatSpaced reflections)
    where
        litTokens = map makeLitToken (literals cf)
        userTokens = map (makeUserToken . wpThing) $
            [name | TokenReg name _ _ <- cfgPragmas cf]
        makeLitToken "Char" = tokenStructHeader "Char" "char"
        makeLitToken "String" =
            tokenStructWithRefConstructorsHeader "String" "std::string"
        makeLitToken "Integer" = tokenStructHeader "Integer" "int"
        makeLitToken "Double" = tokenStructHeader "Double" "double"
        makeLitToken s = -- Ident
            tokenStructWithRefConstructorsHeader s "std::string"
        makeUserToken s =
            tokenStructWithRefConstructorsHeader s "std::string"
        (structs, reflections) = unzip (litTokens ++ userTokens)

implTokens :: CF -> Doc
implTokens cf = vcatSpaced $ litTokens ++ userTokens
    where
        litTokens = map makeLitToken (literals cf)
        userTokens = map (makeUserToken . wpThing) $
            [name | TokenReg name _ _ <- cfgPragmas cf]
        makeLitToken s@"Char" = tokenStructImpl s "char"
        makeLitToken s@"String" =
            tokenStructWithRefConstructorsImpl s "std::string"
        makeLitToken s@"Integer" = tokenStructImpl s "int"
        makeLitToken s@"Double" = tokenStructImpl s "double"
        makeLitToken s = tokenStructWithRefConstructorsImpl s "std::string"
        makeUserToken s = tokenStructWithRefConstructorsImpl s "std::string"

clonePtrImpl :: Doc
clonePtrImpl = linesToText
    [ "template <class T>"
    , "static std::unique_ptr<T> ClonePtr(const std::unique_ptr<T>& p) {"
    , "    if (!p) return {};"
    , "    return std::make_unique<T>(*p);"
    , "}"
    ]

-- | -> (definitions, reflections)
headerCats :: Data.Map.Map Cat [Rule] -> (Doc, Doc)
headerCats groupedRules = (vcatSpaced defs, vcatSpaced refls)
    where
        (defs, refls) = unzip . map todocument . Data.Map.toList
            . mergeCoercCats $ groupedRules
        todocument (cat, rules) =
            let name = catNameNoCoerc cat
            in  case cat of
                ListCat elemCat ->
                    ( text $ "using " ++ name ++ " = std::deque<"
                        ++ catNameNoCoerc elemCat ++ ">;"
                    , rawCoercionSpec name 0 $+$ rawNodeNameSpec name
                    )
                _ ->
                    let ruleNames = filter (/= "_") $ map funName rules
                    in (linesToText $ map (("class "++) . (++";")) ruleNames
                        ++ [ "using " ++ name ++ " = std::variant<"
                           ++ intercalate ", " ruleNames ++ ">;"]
                       , rawNodeNameSpec name
                       )

-- | -> (header, refl, impl)
rules :: CF -> (Doc, Doc, Doc)
rules cf = (vcatSpaced headers, vcatSpaced refls, vcatSpaced impls)
    where (headers, refls, impls) = unzip3 $ map rule $
            filter (not . flip elem ["_", "(:)", "(:[])", "[]", "(++)"]
                    . funName) $ cfgRules cf

-- | -> (header, refl, impl)
rule :: Rule -> (Doc, Doc, Doc)
rule r =
    let name = funName r
        (indexedNames, members) = unzip $ fieldNames $ rhsRule r
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
            -- fields
            $+$ foldr ($+$) empty
                (map (\(typ, name) -> text (typ ++ " " ++ name ++ ";"))
                (zip storageTypes indexedNames)))
            $+$ text "};"

        headerRefls = rawCoercionSpec name (precRule r) $+$ rawNodeNameSpec name

        ctorInitializers = nest 4 . \case
            [] -> empty
            first:rest -> foldr ($+$) empty (text (": " ++ first)
                                            : map (text . (", "++)) rest)
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
            (name ++ "::" ++ name ++ "(const " ++ name ++ "& other)")
            $+$ ctorInitializers
                [name ++ "(" ++ cloneValue cat ("other." ++ name) ++ ")"
                | (name, cat) <- zip indexedNames members] <> " {}"
        copyAsg = text (name ++ "& " ++ name
                         ++ "::operator=(const " ++ name ++ "& other) {")
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
                <> " {}"
        impl = vcatSpaced
            [ text $ "// " ++ name
            , copyCtor
            , copyAsg
            , ruleCtorOrEmpty
            ]

    in (headerClass, headerRefls, impl)
    where
        storageType' :: Cat -> String
        storageType' = \case
            lst@(ListCat _) -> catNameNoCoerc lst
            TokenCat s -> s
            other -> "std::unique_ptr<" ++ catNameNoCoerc other ++ ">"
        isPointerType' :: Cat -> Bool
        isPointerType' = \case
            CoercCat _ _ -> True
            Cat _ -> True
            _ -> False
