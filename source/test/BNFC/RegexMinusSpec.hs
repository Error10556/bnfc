module BNFC.RegexMinusSpec where

import Test.Hspec

import BNFC.RegexMinus

-- | lists remaining suffixes greedily
match :: Regex Char -> String -> [String]
match = \case
    Term ch -> \case
        h:suf -> if h == ch then [suf] else []
        _ -> []
    Lambda -> (:[])
    Phi -> const []
    Rep reg -> helper
        where
            matcher = match reg
            helper s = concatMap helper (matcher s) ++ [s]
    Or a b -> \s -> match a s ++ match b s
    Seq a b -> concatMap (match b) . match a
    Sub _ _ -> error "No Sub promised, but one encountered"

matchFull :: Regex Char -> String -> Bool
matchFull reg s = any null $ match reg s

spec :: Spec
spec = do
    let testcase function s expected
            = it ((if null s then "Empty" else s)
                ++ (if expected then " matches" else " does not match"))
                $ function s `shouldBe` expected

    -- Example that requires loop detection. Taken From:
    --  http://home.chello.no/~mgrsby/sgmlintr/file0005.htm
    describe "(a+b)* - aa*" $ do
        let exp = removeMinuses $
                Sub (Rep (Or (Term 'a') (Term 'b')))
                    (Seq (Term 'a') (Rep (Term 'a')))
            test = testcase (matchFull exp)
        test "" True
        test "a" False
        test "aaA" False
        test "b" True
        test "aaaaaab" True
        test "aaaaaaa" False
        test "baaaaaa" True

    describe "a-b" $ do
        let exp = removeMinuses $ Sub (Term 'a') (Term 'b')
            test = testcase (matchFull exp)
        test "a" True
        test "aa" False
        test "b" False
        test "" False
        test "c" False

    describe "(a+b) - b" $ do
        let exp = removeMinuses $ Sub (Or (Term 'a') (Term 'b')) (Term 'b')
            test = testcase (matchFull exp)
        test "" False
        test "a" True
        test "b" False
        test "ab" False
        test "aa" False
        test "bb" False
        test "ba" False

    describe "(a+b)c - b" $ do
        let exp = removeMinuses
                $ Sub (Seq (Or (Term 'a') (Term 'b')) (Term 'c'))
                      (Term 'b')
            test = testcase (matchFull exp)
        test "" False
        test "ac" True
        test "bc" True
        test "a" False
        test "b" False
        test "c" False
        test "abc" False

    describe "(b*(a+b)c) - b" $ do
        let exp = removeMinuses
                $ Sub (Seq (Rep (Term 'b'))
                           (Seq (Or (Term 'a') (Term 'b')) (Term 'c')))
                      (Term 'b')
            test = testcase (matchFull exp)
        test "" False
        test "ac" True
        test "bc" True
        test "bac" True
        test "bbc" True
        test "bbac" True
        test "bbbc" True
        test "a" False
        test "b" False
        test "c" False
        test "acc" False
        test "bcc" False
        test "bacc" False
        test "bbcc" False
        test "bbacc" False
        test "bbbcc" False
        test "ab" False
        test "bb" False
        test "bab" False
        test "bbb" False
        test "bbab" False
        test "bbbb" False
        test "acb" False

    describe "[abcdefghijklmnopqrstuvwxyz]* - hello" $ do
        let exp = removeMinuses
                $ Sub (Rep (charset "abcdefghijklmnopqrstuvwxyz"))
                      (string "hello")
            test = testcase (matchFull exp)
        test "hello" False
        test "" True
        test "a" True
        test "g" True
        test "sdcskjgnkjren" True
        test "helloworld" True
        test "ahello" True
        test "helloa" True
        test "_" False
        test "a b" False
        test "hell" True
        test "ello" True

    describe "[abcdef]* - fad" $ do
        let exp = removeMinuses $ Sub (Rep (charset "abcdef")) (string "fad")
            test = testcase (matchFull exp)
        test "fad" False
        test "" True
        test "a" True
        test "f" True
        test "deadbeef" True
        test "feeddad" True
        test "afad" True
        test "fada" True
        test "_" False
        test "a b" False
        test "fa" True
        test "ad" True

    describe "[abcdef]* - b" $ do
        let exp = removeMinuses $ Sub (Rep (charset "abcdef")) (string "b")
            test = testcase (matchFull exp)
        test "" True
        test "a" True
        test "b" False
        test "c" True
        test "d" True
        test "e" True
        test "f" True
        test "deadbeef" True
        test "feeddad" True
        test "ab" True
        test "ba" True
        test "_" False
        test "a d" False
