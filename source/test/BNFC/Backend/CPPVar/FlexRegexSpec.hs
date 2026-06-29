module BNFC.Backend.CPPVar.FlexRegexSpec where

import Test.Hspec

import BNFC.PrettyPrint

import qualified BNFC.Backend.CPPVar.FlexRegex as RX
import Control.Exception (evaluate)
import Data.Char
import Numeric
import BNFC.RegexMinus
import Data.List (sort)
import Data.Int (Int8)
import BNFC.Backend.CPPVar.FlexRegex (simpleCharsetUTF8)

spec :: Spec
spec = do
    describe "pretty-printing regexes" $ do
        let test str reg = it str $ show (pretty reg) `shouldBe` str

        test "a|b|c|[^d]" $ RX.flexOr [RX.byteset "a", RX.onebyte 'b',
            RX.onebyte 'c', RX.byteset (['\0'..'c'] ++ ['e'..'\xff'])]
        test "[ab]**" $ RX.Star $ RX.Star $ RX.byteset "ab"
        test "(a|b)+" $ RX.Plus $ RX.Or (RX.onebyte 'a') (RX.onebyte 'b')
        test "(\\(\\.\\*\\?\\))*\\+2" $
            RX.Concat (RX.Star (RX.flexConcat (map RX.onebyte "(.*?)"))) $
                RX.flexConcat $ map RX.onebyte "+2"
        test "[0-9A-Za-z:;<=>?@\\x5b\\x5c\\x5d\\x5e_`]"
            $ RX.byteset ['0'..'z']
        test "\\:;\\<=\\>\\?@\\[\\\\\\]\\^_`"
            $ RX.flexConcat $ map RX.Onebyte $ [58..64] ++ [91..96]
        test "\\^" $ RX.byteset ['^']
        test "[\\x5e\\xff]" $ RX.byteset ['^', '\xff']
        test "\\0\\x01\\x02\\a\\b\\t\\n" $
            RX.flexConcat $ map RX.onebyte "\x00\x01\x02\x07\x08\x09\x0a"
        test "a\"\"*(a\"\")*" $ RX.Concat
            (RX.Concat (RX.onebyte 'a') (RX.Star RX.Empty))
            (RX.Star $ RX.Concat (RX.onebyte 'a') RX.Empty)
        test "0*+??+" $ RX.Plus . RX.Optional . RX.Optional . RX.Plus .
            RX.Star . RX.onebyte $ '0'
        test "a(a|bc|d)(a|b)(c|d)" $ RX.Concat (RX.onebyte 'a')
            (RX.Concat
                (RX.Concat
                    (RX.Or
                        (RX.Or (RX.onebyte 'a')
                            (RX.flexConcat $ map RX.onebyte "bc"))
                        (RX.onebyte 'd'))
                    (RX.Or (RX.onebyte 'a') (RX.onebyte 'b')))
                (RX.Or (RX.onebyte 'c') (RX.onebyte 'd')))

    describe "Conversion functions throw" $ do
        it "onebyte: bytes don't throw" $
            show (map RX.onebyte ['\0'..'\xff'])
                `shouldSatisfy` (\s -> length s >= 2)
        it "onebyte: char 256 throws" $
            evaluate (let (RX.Onebyte b) = RX.onebyte (chr 256) in b)
                `shouldThrow` anyException

        it "byteset: bytes don't throw" $
            -- foldr instead of Data.Set.toList because can't import Data.Set
            (\(RX.Byteset set) -> show . foldr (:) [] $ set)
                (RX.byteset ['\0'..'\xff'])
                `shouldSatisfy` (\s -> length s >= 2)
        it "byteset: char 256 throws" $
            evaluate (let (RX.Byteset s) = RX.byteset "\x100" in s)
                `shouldThrow` anyException
        it "byteset: char 2560 throws" $
            evaluate (let (RX.Byteset s) = RX.byteset "\xA00" in s)
                `shouldThrow` anyException

    describe "byte2char in normal context" $ do
        let normal = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_@#`'~&%=;"
            ordnormal = map ord normal
            needescape = ".+?*<>(){}[]$^/:\"\\"
            ordneedescape = map ord needescape
            special = [0, 7, 8, 9, 10, 11, 12, 13]
            other = minus (ordnormal ++ ordneedescape ++ special)
        it "maps correctly" $
            concatMap (RX.byte2char False)
                (map fromIntegral
                    $ ordnormal ++ ordneedescape ++ special ++ other)
                `shouldBe`
                (normal
                ++ foldr (\ch tail -> '\\' : ch : tail) [] needescape
                ++ "\\0\\a\\b\\t\\n\\v\\f\\r"
                ++ concatMap (("\\x"++) . pad2 . flip showHex "") other)

    describe "byte2char in charclass context" $ do
        let normal = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++
                "_@#`'~&%=;.+?*<>(){}$/:;\"|"
            ordnormal = map ord normal
            special = [0, 7, 8, 9, 10, 11, 12, 13]
            other = minus (ordnormal ++ special)
        it "maps correctly" $
            concatMap (RX.byte2char True)
                (map fromIntegral $ ordnormal ++ special ++ other)
                `shouldBe`
                (normal
                ++ "\\0\\a\\b\\t\\n\\v\\f\\r"
                ++ concatMap (("\\x"++) . pad2 . flip showHex "") other)

    describe "Conversion from RegexMinus" $ do
        let
            test str minus = (show . pretty . RX.fromMinusRegex $ minus)
                `shouldBe` str
            byteterm = Term . fromIntegral . ord :: Char -> SimpleRegex Int8
        -- sort "qweйцу" = "eqwйуц"
        -- "йуц" in utf8 in hex is d0b9 d183 d186
        it "UTF8" $ test "[eqw]|\\xd0\\xb9|\\xd1\\x83|\\xd1\\x86"
            $ simpleCharsetUTF8 "qweйцу"
        it "aa* -> a+" $ test "a+"
            $ byteterm 'a' `Seq` Rep (byteterm 'a')
        it "(a|bc)(a|bc)* -> (a|bc)+" $ test "(a|bc)+" $
            let abc = byteterm 'a' `Or` RX.simpleConcatUTF8 "bc"
            in Seq abc (Rep abc)
        -- Phi = empty language
        it "r|Phi -> r" $ test "r" $ Or (RX.simpleConcatUTF8 "r") Phi
        it "a|b|Phi|c -> [a-c]" $ test "[a-c]"
            $ Or (byteterm 'a') $ Or (byteterm 'b') $ Or Phi $ byteterm 'c'
        it "r|\"\" -> r?" $ test "r?" $ Or (byteterm 'r') Lambda
        it "a|b|\"\"|c -> [a-c]?" $ test "[a-c]?" $ Or (byteterm 'a')
            $ Or (byteterm 'b') $ Or Lambda $ byteterm 'c'
        it "a|b|\"\"|Phi|cd -> ([ab]|cd)?" $ test "([ab]|cd)?" $ Or (byteterm 'a') $
            Or (byteterm 'b') $ Or Lambda $ Or Phi $ Seq (byteterm 'c') (byteterm 'd')
        it "a\"\", \"\"a -> a" $ test "ac" $ Seq (byteterm 'a') $ Seq Lambda $
            byteterm 'c'
        it "aPhi, Phia -> Phi" $ test "[^\\0-\\xff]" $ Seq (byteterm 'a') $
            Seq (byteterm 'b') $ Seq Phi $ byteterm 'c'
        it "\"\"* -> \"\"" $ test "\"\"" $ Rep Lambda

        it "a([c] - [^c])b -> acb" $ test "acb" $ Seq (byteterm 'a') $
            Seq (Sub (simpleCharsetUTF8 "c")
                    (simpleCharsetUTF8 (['\0'..'b'] ++ ['d'..'\xff']))) $
            byteterm 'b'
        it "a([a-z] - [a-k] - [l-z])b -> Phi" $ test "[^\\0-\\xff]" $
            Seq (byteterm 'a')
            $ Seq (Sub (Sub (simpleCharsetUTF8 ['a'..'z'])
                            (simpleCharsetUTF8 ['a'..'k']))
                       (simpleCharsetUTF8 ['l'..'z']))
            $ byteterm 'b'

    where
        pad2 s = take (2 - length s) ['0'..] ++ s
        -- | [0..255] - ls
        minus = helper 0 . uniq (-1) . sort
            where
                helper i ls = if i == 256 then [] else
                    case ls of
                        [] -> [i..255]
                        x:xs -> if i == x then helper (i + 1) xs
                            else i : helper (i + 1) ls
                uniq prev = \case
                    [] -> []
                    x:xs -> (if x == prev then id else (x:)) $ uniq x xs
