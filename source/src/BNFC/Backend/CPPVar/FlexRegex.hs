module BNFC.Backend.CPPVar.FlexRegex
  ( FlexRegex(..)
  , byteset, onebyte, flexConcat, flexOr
  , byte2char, bytecharClass, bytecharRanges
  , flexConcatUTF8
  , flexCharsetUTF8
  , simpleConcatUTF8
  , simpleCharsetUTF8
  , flexRegexPrecedence
  , precedenceEmpty
  , precedenceOnebyte
  , precedenceByteset
  , precedenceStar
  , precedenceOptional
  , precedencePlus
  , precedenceConcat
  , precedenceOr
  , fromMinusRegex
  , fromBNFCReg
  ) where

import qualified Data.Set
import qualified BNFC.PrettyPrint
import qualified BNFC.RegexMinus as Minus
import Data.Char
import Numeric
import Data.Bits
import Data.Either
import Data.List (delete, partition)
import Data.Int (Int8)
import qualified BNFC.Abs

data FlexRegex
  = Empty  -- ^ ""
  | Onebyte Int8  -- ^ technically Onebyte ch ~= Byteset (fromList [ch])
  | Byteset (Data.Set.Set Int8)
  | Star FlexRegex
  | Optional FlexRegex
  | Plus FlexRegex
  | Concat FlexRegex FlexRegex
  | Or FlexRegex FlexRegex
  deriving (Show, Ord, Eq)

onebyte :: Char -> FlexRegex
onebyte ch = Onebyte o
  where
    _o = ord ch
    o = if 0 <= _o && _o <= 255 then fromIntegral _o else
      error "onebyte called with a non-byte character"

byteset :: String -> FlexRegex
byteset s = Byteset $ Data.Set.fromList ords
  where
    _ords = map ord s
    ords = if all (\i -> 0 <= i && i <= 255) _ords
      then map fromIntegral _ords else
      error "byteset called with a non-byte-character string"

flexConcat :: [FlexRegex] -> FlexRegex
flexConcat = \case
  [] -> Empty
  nonempty -> foldr1 Concat nonempty

flexOr :: [FlexRegex] -> FlexRegex
flexOr = \case
  [] -> Byteset $ Data.Set.fromList []
  nonempty -> foldr1 Or nonempty

flexConcatUTF8 :: String -> FlexRegex
flexConcatUTF8 = flexConcat . map Onebyte . concatMap (utf8encode . ord)

flexCharsetUTF8 :: String -> FlexRegex
flexCharsetUTF8 = flexOr . map (flexConcat . map Onebyte . utf8encode . ord)

simpleConcatUTF8 :: String -> Minus.SimpleRegex Int8
simpleConcatUTF8 = Minus.string . concatMap (utf8encode . ord)

simpleCharsetUTF8 :: String -> Minus.SimpleRegex Int8
simpleCharsetUTF8 =
  foldr Minus.Or Minus.Phi . map (Minus.string . utf8encode . ord)

instance BNFC.PrettyPrint.Pretty FlexRegex where
  pretty = \case
      Empty -> text "\"\""
      Onebyte byte -> text $ byte2char False byte
      Byteset set -> text $
        displayByteset (Data.Set.toList set)
      Star regex -> prettyPrec precedenceStar regex
        BNFC.PrettyPrint.<> text "*"
      Optional regex -> prettyPrec precedenceOptional regex
        BNFC.PrettyPrint.<> text "?"
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

hexByte :: Int8 -> String
hexByte byte = let int = fromIntegral byte :: Int in
  leftPad $ showHex (if int < 0 then 256 + int else int) ""
  where
    leftPad s = case length s of
      0 -> "00"
      1 -> '0':s
      2 -> s
      _ -> error ("Not a byte: 0x" ++ s)

byte2char :: Bool -> Int8 -> String
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
  | otherwise = "\\x" ++ hexByte byte
  where
    safeBytes :: Data.Set.Set Int8
    safeBytes = Data.Set.fromList . map (fromIntegral . ord) $
      ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_@#`'~&%=;"
    safeInCharclass :: Data.Set.Set Int8
    safeInCharclass = Data.Set.fromList . map (fromIntegral . ord) $
      ".+?*<>(){}$/:;\"|"
    escapedOutOfCharclass :: Data.Set.Set Int8
    escapedOutOfCharclass = Data.Set.fromList . map (fromIntegral . ord) $
      ".+?*<>(){}[]$^/:\"\\"

-- | Decides between [^...] and [...], uses bytecharRanges
-- REQUIRES a sorted list as input
bytecharClass :: [Int8] -> String
bytecharClass [] = "[^\\0-\\xff]"
bytecharClass bytes
  | bytes == [-128..127] = "[\\0-\\xff]"
  | otherwise = let
      (intsNeg, intsPos) = partition (< 0) $ map fromIntegral bytes
        :: ([Int], [Int])
      ints = intsPos ++ map (+256) intsNeg
      normal = bytecharRanges ints
      inv = bytecharRanges $ invertByteset [0..255] ints
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
  ++ escranges nonascii
  where
    (bad1, tail1) = span (< ord '0') bytes
    (digits, tail2) = span (<= ord '9') tail1
    (bad2, tail3) = span (< ord 'A') tail2
    (caps, tail4) = span (<= ord 'Z') tail3
    (bad3, tail5) = span (< ord 'a') tail4
    (lows, tail6) = span (<= ord 'z') tail5
    (bad4, nonascii) = span (<= 127) tail6
    saferanges :: [Int] -> String
    saferanges = concatMap (uncurry range2str) . getranges
      where
        range2str start end
          | start == end = [chr start]
          | start + 1 == end = [chr start, chr end]
          | otherwise = [chr start, '-', chr end]
    escranges = concatMap (uncurry escRange2str) . getranges
      where
        escRange2str start end
          | start == end = escHex start
          | start + 1 == end = concatMap escHex [start, end]
          | otherwise = escHex start ++ ('-' : escHex end)
        escHex :: Int -> String
        escHex = ("\\x" ++) . hexByte . fromIntegral
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

precedenceEmpty :: Int
precedenceEmpty = 3
precedenceOnebyte :: Int
precedenceOnebyte = 3
precedenceByteset :: Int
precedenceByteset = 3
precedenceStar :: Int
precedenceStar = 2
precedenceOptional :: Int
precedenceOptional = 2
precedencePlus :: Int
precedencePlus = 2
precedenceConcat :: Int
precedenceConcat = 1
precedenceOr :: Int
precedenceOr = 0

flexRegexPrecedence :: FlexRegex -> Int
flexRegexPrecedence = \case
  Empty -> precedenceEmpty
  Onebyte _ -> precedenceOnebyte
  Byteset _ -> precedenceByteset
  Star _ -> precedenceStar
  Optional _ -> precedenceOptional
  Plus _ -> precedencePlus
  Concat _ _ -> precedenceConcat
  Or _ _ -> precedenceOr

-- | encodes a character with code c into a list of bytes.
-- See 'man 7 utf8'
-- Don't think we need a library just for this
utf8encode :: Int -> [Int8]
utf8encode = map fromIntegral . helper
  where
    helper c
      | c < 0 = error "Negative char"
      | c <= 0x7f = [c]
      | c <= 0x7ff = [0xc0 + shiftR6 1 c, 0x80 + (c .&. 0x3F)]
      | c <= 0xffff =
        [0xe0 + shiftR6 2 c, 0x80 + shiftR6 1 c, 0x80 + (c .&. 0x3f)]
      | c <= 0x1fffff =
        [ 0xf0 + shiftR6 3 c
        , 0x80 + shiftR6 2 c
        , 0x80 + shiftR6 1 c
        , 0x80 + (c .&. 0x3f)
        ]
      | c <= 0x3ffffff =
        [ 0xf8 + shiftR6 4 c
        , 0x80 + shiftR6 3 c
        , 0x80 + shiftR6 2 c
        , 0x80 + shiftR6 1 c
        , 0x80 + (c .&. 0x3f)
        ]
      | otherwise =
        [ 0xfc + shiftR6 5 c
        , 0x80 + shiftR6 4 c
        , 0x80 + shiftR6 3 c
        , 0x80 + shiftR6 2 c
        , 0x80 + shiftR6 1 c
        , 0x80 + (c .&. 0x3f)
        ]
      where
        shiftR6 n c = (c `shiftR` (6 * n)) .&. 0x3f

-- | Removes minuses from regexes
-- simplifies:
-- (1) aa* -> a+
-- (2) r|Phi -> r
-- (3) r|"" -> r?
-- (4) tree of Seq -> list
-- (5) a"", ""a -> a
-- (6) aPhi, Phia -> Phi
-- (7) tree of Or -> set
-- (8) [set1]|[set2] -> [set1set2]
-- (9) ""* -> ""
-- (10) Phi* -> Phi
fromMinusRegex :: Minus.SimpleRegex Int8 -> FlexRegex
fromMinusRegex simplereg = case Minus.removeMinuses simplereg of
  Minus.Term byte -> Onebyte $ fromIntegral byte
  Minus.Lambda -> Empty
  Minus.Phi -> byteset ""
  Minus.Rep r -> case fromMinusRegex r of
      Empty -> Empty  -- (9)
      bs@(Byteset set) -> if null set then bs else Star bs  -- (10)
      other -> Star other
  or@(Minus.Or _ _) -> let
      regexSet = makeRegexSetFromOr or  -- (7)
      (bytesets, nonbytesets) = partitionEithers . map
        (\case
          Byteset set -> Left set
          Onebyte b -> Left (Data.Set.singleton b)
          other -> Right other) $
        Data.Set.toList regexSet
      onebyteset = Data.Set.unions bytesets  -- (2) (8)
      unifiedRegexes = if null onebyteset then nonbytesets
        else Byteset onebyteset : nonbytesets
    -- (Empty `elem` unifiedRegexes) iff it is an `elem` of regexSet
    in if Empty `elem` regexSet
      then Optional . flexOr . delete Empty $ unifiedRegexes  -- (3)
      else flexOr unifiedRegexes
  Minus.Sub _ _ -> error "removeMinuses returned a Sub"
  seq@(Minus.Seq _ _) -> let
      regexList = makeRegexListFromSeq seq  -- (4)
      -- | (5)
      listNoEmpty = filter (\case Empty -> False; _ -> True) regexList
      emptyset = byteset ""
    in if emptyset `elem` listNoEmpty then emptyset  -- (6)
      else flexConcat $ foldr foldStar2Plus [] listNoEmpty -- (1)
  where
    makeRegexSetFromOr = \case
      Minus.Or a b -> Data.Set.union
        (makeRegexSetFromOr a) (makeRegexSetFromOr b)
      reg -> Data.Set.singleton $ fromMinusRegex reg
    makeRegexListFromSeq = \case
      Minus.Seq a b -> makeRegexListFromSeq a ++ makeRegexListFromSeq b
      reg -> [fromMinusRegex reg]
    foldStar2Plus elem = \case -- case tail of
      [] -> [elem]
      tail@(Star reg : xs) -> if reg == elem then Plus elem : xs
        else elem : tail
      tail -> elem : tail

fromBNFCReg :: BNFC.Abs.Reg -> Minus.SimpleRegex Int8
fromBNFCReg = Minus.toSimpleRegex
  (Minus.string . utf8encode . ord) -- char2regex
  anyUTF8 -- any
  (Minus.charset [byte0..byte9]) -- digit
  (Minus.charset ([byte_A..byte_Z] ++ [byte_a..byte_z])) -- letter
  (Minus.charset ([byte_A..byte_Z])) -- upper
  (Minus.charset ([byte_a..byte_z])) -- lower
  where
    byte0 = fromIntegral (ord '0') :: Int8
    byte9 = fromIntegral (ord '9') :: Int8
    byte_a = fromIntegral (ord 'a') :: Int8
    byte_z = fromIntegral (ord 'z') :: Int8
    byte_A = fromIntegral (ord 'A') :: Int8
    byte_Z = fromIntegral (ord 'Z') :: Int8
    byterange :: Int -> Int -> Minus.SimpleRegex Int8
    byterange from to = Minus.charset (map fromIntegral [from..to] :: [Int8])
    utf8Continuation :: Int -> Minus.SimpleRegex Int8
    utf8Continuation n = foldr1 Minus.Seq $ replicate n $ byterange 0x80 0xbf
    anyUTF8 = foldr1 Minus.Or
      [ byterange 0 127
      , byterange 0xc0 0xdf `Minus.Seq` utf8Continuation 1
      , byterange 0xe0 0xef `Minus.Seq` utf8Continuation 2
      , byterange 0xf0 0xf7 `Minus.Seq` utf8Continuation 3
      , byterange 0xf8 0xfb `Minus.Seq` utf8Continuation 4
      , byterange 0xfc 0xfd `Minus.Seq` utf8Continuation 5
      ]
