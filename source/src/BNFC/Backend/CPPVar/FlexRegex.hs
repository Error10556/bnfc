module BNFC.Backend.CPPVar.FlexRegex (FlexRegex(..)) where

import qualified Data.Set
import qualified BNFC.PrettyPrint
-- import qualified BNFC.RegexMinus
import Data.Char
import Numeric

data FlexRegex
    = Onebyte Int
    | Byteset (Data.Set.Set Int)
    | Star FlexRegex
    | Optional FlexRegex
    | Plus FlexRegex
    | Concat FlexRegex FlexRegex
    | Or FlexRegex FlexRegex
    deriving (Show)

instance BNFC.PrettyPrint.Pretty FlexRegex where
    pretty = \case
            Onebyte byte -> text $ byte2char False byte
            Byteset set -> text $
                displayByteset (Data.Set.toList set)
            Star regex -> prettyPrec precedenceStar regex
                BNFC.PrettyPrint.<> text "*"
            Optional regex -> prettyPrec precedenceOptional regex
                BNFC.PrettyPrint.<> text "*"
            Plus regex -> prettyPrec precedencePlus regex
                BNFC.PrettyPrint.<> text "+"
            Concat r1 r2 -> prettyPrec precedenceConcat r1
                BNFC.PrettyPrint.<> prettyPrec precedenceConcat r2
            Or r1 r2 -> prettyPrec precedenceOr r1 BNFC.PrettyPrint.<> text "|"
                BNFC.PrettyPrint.<> prettyPrec precedenceOr r2
        where
            displayByteset chars
                | length chars == 1 = let [ch] = chars in byte2char False ch
                | otherwise = bytecharClass chars
            prettyPrec = BNFC.PrettyPrint.prettyPrec
                :: (Int -> FlexRegex -> BNFC.PrettyPrint.Doc)
            text = BNFC.PrettyPrint.text
    prettyPrec prec regex = if flexRegexPrecedence regex < prec
        then BNFC.PrettyPrint.text "(" BNFC.PrettyPrint.<> s
            BNFC.PrettyPrint.<> BNFC.PrettyPrint.text ")"
        else s
        where s = BNFC.PrettyPrint.pretty regex

byte2char :: Bool -> Int -> String
byte2char isInCharclass byte
    | byte == 0 = "\\0"
    | byte == 7 = "\\a"  -- bell/alarm
    | byte == 8 = "\\b"  -- backspace
    | byte == 12 = "\\f" -- form feed
    | byte == 10 = "\\n"
    | byte == 13 = "\\r"
    | byte == 9 = "\\t"
    | byte == 11 = "\\v" -- vertical tab
    | byte `elem` safeBytes = [chr (fromIntegral byte)]
    | isInCharclass && byte `elem` safeInCharclass = [chr (fromIntegral byte)]
    | not isInCharclass && byte `elem` escapedOutOfCharclass
        = ['\\', chr (fromIntegral byte)]
    | otherwise = "\\x" ++ case showHex byte "" of
        [] -> "00"
        s@[_] -> '0' : s
        s@[_,_] -> s
        s -> error $ "not a byte: 0x" ++ s
    where
        safeBytes :: Data.Set.Set Int
        safeBytes = Data.Set.fromList . map (fromIntegral . ord) $
            ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_@#`'~&%"
        safeInCharclass :: Data.Set.Set Int
        safeInCharclass = Data.Set.fromList . map (fromIntegral . ord) $
            ".+?*<>(){}$/:;\"|"
        escapedOutOfCharclass :: Data.Set.Set Int
        escapedOutOfCharclass = Data.Set.fromList . map (fromIntegral . ord) $
            ".+?*<>(){}[]$^/:;\"\\"

-- | Decides between [^...] and [...]
-- REQUIRES a sorted list as input
bytecharClass :: [Int] -> String
bytecharClass bytes =
    let normal = bytecharRanges bytes
        inv = bytecharRanges $ invertByteset [0..255] bytes
    in if length normal <= length inv + 1
        then "[" ++ normal ++ "]"
        else "[^" ++ inv ++ "]"
    where
        invertByteset :: [Int] -> [Int] -> [Int]
        invertByteset all orig = case all of
            [] -> []
            cur:all' -> case orig of
                [] -> cur : all'
                curorig:orig' -> if cur == curorig
                    then invertByteset all' orig'
                    else cur : invertByteset all' orig

-- | e.g. [ord '1'..ord '9'] ++ [ord 'h'] -> "1-9h"
-- flex discourages ranging over different classes, so we cannot
-- return ranges like [A-z]
-- REQUIRES a sorted list as input
bytecharRanges :: [Int] -> String
bytecharRanges bytes = concatMap saferanges [digits, caps, lows]
    ++ concatMap (byte2char True . fromIntegral) (bad1 ++ bad2 ++ bad3 ++ bad4)
    where
        (bad1, tail1) = span (< ord '0') bytes
        (digits, tail2) = span (<= ord '9') tail1
        (bad2, tail3) = span (< ord 'A') tail2
        (caps, tail4) = span (<= ord 'Z') tail3
        (bad3, tail5) = span (< ord 'a') tail4
        (lows, bad4) = span (<= ord 'z') tail5
        saferanges :: [Int] -> String
        saferanges = concatMap (uncurry range2str) . getranges
            where
                range2str start end
                    | start == end = [chr start]
                    | start + 1 == end = [chr start, chr end]
                    | otherwise = [chr start, '-', chr end]
        getranges :: [Int] -> [(Int, Int)]
        getranges = \case
            [] -> []
            (fst:rest) -> helper fst fst rest
            where
                helper start prev = \case
                    [] -> [(start, prev)]
                    (nxt:tail) -> if prev + 1 == nxt
                        then helper start nxt tail
                        else (start, prev) : helper nxt nxt tail

precedenceOnebyte :: Int
precedenceOnebyte = 2
precedenceByteset :: Int
precedenceByteset = 2
precedenceStar :: Int
precedenceStar = 1
precedenceOptional :: Int
precedenceOptional = 1
precedencePlus :: Int
precedencePlus = 1
precedenceConcat :: Int
precedenceConcat = 1
precedenceOr :: Int
precedenceOr = 0

flexRegexPrecedence :: FlexRegex -> Int
flexRegexPrecedence = \case
    Onebyte _ -> precedenceOnebyte
    Byteset _ -> precedenceByteset
    Star _ -> precedenceStar
    Optional _ -> precedenceOptional
    Plus _ -> precedencePlus
    Concat _ _ -> precedenceConcat
    Or _ _ -> precedenceOr
