{-# LANGUAGE MultilineStrings #-}

module BNFC.Backend.CPPVar.AbsynGen
    (makeAbsyn, absynHppFilename, absynCppFilename) where

--import BNFC.Utils
import BNFC.CF
import BNFC.Options
import Text.PrettyPrint (Doc, text, empty, ($+$))
import BNFC.Backend.CPPVar.CPPUtil

absynHppFilename :: String
absynHppFilename = "Absyn.hpp"

absynCppFilename :: String
absynCppFilename = "Absyn.cpp"

-- | returns (Absyn.hpp, Absyn.cpp)
makeAbsyn :: SharedOptions -> CF -> (Doc, Doc)
makeAbsyn opts cf = (hpp, cpp)
    where
        maybeNamespace = maybe id wrapNamespace (inPackage opts)
        hpp = headerHead $++$ maybeNamespace (headerTokens cf)
        cpp = text ("#include \"" ++ absynHppFilename ++ "\"")
            $++$ maybeNamespace
            (clonePtrImpl $++$ implTokens cf)

headerHead :: Doc
headerHead = text """
#pragma once
#include <memory>
#include <string>
#include <vector>
#include <variant>
"""

reflectionTemplates :: Doc
reflectionTemplates = text """
template <class T> struct CoercionLevel_t {};
template<class T> constexpr int CoercionLevel = CoercionLevel_t<T>::value;

template <class T> struct SyntaxNodeName_t {};
template<class T>
constexpr const char* SyntaxNodeName = SyntaxNodeName_t<T>::value;
"""

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

headerTokens :: CF -> Doc
headerTokens cf = foldr ($++$) empty structs
    $++$ wrapNamespace "reflection"
    (reflectionTemplates $++$ foldr ($+$) empty reflections)
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
implTokens cf = foldr ($++$) empty $ litTokens ++ userTokens
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
clonePtrImpl = text """
template <class T>
static std::unique_ptr<T> ClonePtr(const std::unique_ptr<T>& p) {
    if (!p) return {};
    return std::make_unique<T>(*p);
}
"""
