module BNFC.Backend.CPPVar.PrinterUtils where

import Prelude hiding (lookup)
import BNFC.CF
import BNFC.Backend.CPPVar.CPPUtil
import Data.Map hiding (map, foldr, filter)

data PrintableSymbol
    = NormalCategory String
    | ListCategory
        { printListEmpty :: Maybe [String]
        , printListCons :: Maybe ([String], Cat, [String], Cat, [String])
        , printListSingle :: Maybe ([String], Cat, [String])
        }
    | FunctionRule Rule
    | Ident
    -- TODO add String, Char, Double, Integer

literalName2Symbol :: Map Literal PrintableSymbol
literalName2Symbol = fromList [(catIdent, Ident)]

getPrintableSymbols :: CF -> GroupedRules -> [PrintableSymbol]
getPrintableSymbols cf rulemap = literals ++ nonliterals
    where
        literals = map (\name -> case name `lookup` literalName2Symbol of
            Just printable -> printable
            Nothing -> error $ "Unsupported literal: " ++ name)
            $ cfgLiterals cf
        nonliterals = concat
            $ flip map (toList (mergeCoercCats rulemap)) $ \case
            (ListCat _, rules) -> [ListCategory
                { printListEmpty = parseEmptyList <$> lookup "[]" mapRules
                , printListCons = parseCons <$> lookup "(:)" mapRules
                , printListSingle = parseSingleton <$> lookup "(:[])" mapRules
                }]
                where mapRules = fromList [(funName r, rhsRule r) | r <- rules]
            (cat@(Cat _), rules) ->
                makeNormalCategory (catNameNoCoerc cat) rules
            (cat@(CoercCat _ _), rules) ->
                makeNormalCategory (catNameNoCoerc cat) rules
            _ -> error "TokenCat in GroupedRules"
        makeNormalCategory :: String -> [Rule] -> [PrintableSymbol]
        makeNormalCategory catname rules = NormalCategory catname :
            map FunctionRule (filter (\r -> funName r /= "_") rules)
        readStrings :: SentForm -> ([String], SentForm)
        readStrings sentForm = case sentForm of
            [] -> ([], sentForm)
            (Left _):_ -> ([], sentForm)
            (Right s):tail -> let (ss, resform) = readStrings tail
                in (s : ss, resform)
        -- | returns n Cats and (n+1) [String]s, reading interleaved
        parseSentForm :: Int -> SentForm -> ([Cat], [[String]])
        parseSentForm n sentForm
            | n == 0 = case tail of
                [] -> ([], [strings])
                _ -> error "Too many categories in a sentence form"
            | otherwise = case tail of
                (Left cat):tail2 ->
                    let (cats', strs') = parseSentForm (n - 1) tail2
                    in (cat : cats', strings : strs')
                _ -> error "Too few categories in a sentence form"
            where
                (strings, tail) = readStrings sentForm
        parseEmptyList :: SentForm -> [String]
        parseEmptyList sentForm =
            let ([], [ss]) = parseSentForm 0 sentForm in ss
        parseSingleton :: SentForm -> ([String], Cat, [String])
        parseSingleton sentForm =
            let ([c], [ss1, ss2]) = parseSentForm 1 sentForm
            in (ss1, c, ss2)
        parseCons :: SentForm -> ([String], Cat, [String], Cat, [String])
        parseCons sentForm =
            let ([c1, c2], [ss1, ss2, ss3]) = parseSentForm 2 sentForm
            in (ss1, c1, ss2, c2, ss3)
