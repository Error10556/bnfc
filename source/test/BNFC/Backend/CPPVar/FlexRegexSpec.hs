module BNFC.Backend.CPPVar.FlexRegexSpec where

import Test.Hspec

import BNFC.PrettyPrint

import qualified BNFC.Backend.CPPVar.FlexRegex as RX

f :: Pretty a => a -> Doc
f = pretty

spec :: Spec
spec = do
    describe "pretty-printing regexes" $ do
        let test str reg = it str $ show (pretty reg) `shouldBe` str
        
        test "a|b|c|[^d]" $ RX.or [RX.byteset "a", RX.onebyte 'b',
            RX.onebyte 'c', RX.byteset (['\0'..'c'] ++ ['e'..'\xff'])]
        test "[ab]**" $ RX.Star $ RX.Star $ RX.byteset "ab"
        test "(a|b)+" $ RX.Plus $ RX.Or (RX.onebyte 'a') (RX.onebyte 'b')
        test "(\\(\\.\\*\\?\\))*\\+2" $
            RX.Concat (RX.Star (RX.concat (map RX.onebyte "(.*?)"))) $
                RX.concat $ map RX.onebyte "+2"
        test "[0-9A-Za-z:;<=>?@\\x5b\\x5c\\x5d\\x5e_`]"
            $ RX.byteset ['0'..'z']
        test "\\:;\\<=\\>\\?@\\[\\\\\\]\\^_`"
            $ RX.concat $ map RX.Onebyte $ [58..64] ++ [91..96]
        test "\\^" $ RX.byteset ['^']
        test "[\\x5e\\xff]" $ RX.byteset ['^', '\xff']
        test "\\0\\x01\\x02\\a\\b\\t\\n" $
            RX.concat $ map RX.onebyte "\x00\x01\x02\x07\x08\x09\x0a"
