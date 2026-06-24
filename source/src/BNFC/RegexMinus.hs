-- Inspired by David J. Sankel (camior@gmail.com)
-- Many ideas taken from:
-- http://home.chello.no/~mgrsby/sgmlintr/file0003.htm
-- http://home.chello.no/~mgrsby/sgmlintr/file0004.htm
-- http://home.chello.no/~mgrsby/sgmlintr/file0005.htm
-- (accessible via the Wayback Machine)
-- License: Public Domain

module BNFC.RegexMinus
  ( SimpleRegex(..), charset, string, toSimpleRegex
  , removeMinuses, regexToString) where

-- This module is intended to be imported qualified to use in lexer backends
-- which wish to support general subtraction.

{- EXPLANATION
 -
 - First, we define:
 -  λ = the empty string ""
 -  φ = the empty language []
 -  δR (for some regex R) = if λ∊R then λ else φ
 -    For example, δ(a*b*) = λ; δ(ab*) = φ
 -  The derivative Dc(R) (for some character c and regex R) = a regex S such
 -    that S matches exactly the strings that are in R and start with 'c', but
 -    without the first character 'c'.
 -    For example, Da((ab|cd)*) = b(ab|cd)*; Da(a*) = a*; Da(λ) = φ
 -
 - Also note that: λR = Rλ = R; φR = Rφ = φ; R|φ = R; R-φ = R; φ-R = φ;
 -   Dc(R-S)=DcR-DcS
 -
 - The first observation is that if R contains strings that start with
 - 'a', 'b', ..., 'z', then
 -                    R = a(DaR)|b(DbR)|...|z(DzR)|δR
 - The second observation is that, for some regex A that does not match the
 - empty string (δA=φ),
 -                 R = AR|B  if and only if  R = (A*)B
 -
 - Our strategy is to derive the subtraction until we arrive at the same
 - subtraction as a subexpression, and then apply the second observation:
 -
 - Let's consider an example: we have to convert (a|b)*-a* to an equivalent
 - regex without the subtraction operator.
 -
 - R = (a|b)*-a* = a((a|b)*-a)|b((a|b)*-φ) = aR|b(a|b)* = a*b(a|b)*.
 -
 - The derivation might take several steps and/or require grouping several
 - sequences of characters, which is taken into account in this implementation:
 -
 - R = (ab|ac)*-(ac)* = a((b|c)(ab|ac)*-c(ac)*)
 -   = a(b((ab|ac)*-φ)|c((ab|ac)*-(ac)*)) = ab(ab|ac)*|acR=(ac)*ab(ab|ac)*
 -
 - This implementation assigns a unique ID to every regex and uses the IDs to
 - detect "loops".
 -}

import qualified BNFC.Abs as Abs
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

data SimpleRegex a
  = Term a
  | Lambda -- ^ This is the 0-length string
  | Phi    -- ^ Recognizes no strings
  | Rep (SimpleRegex a)  -- ^ Kleene Star (*)
  | Or (SimpleRegex a) (SimpleRegex a) -- ^ Or (|)
  | Sub (SimpleRegex a) (SimpleRegex a) -- ^ SimpleRegex Subtraction (-)
  | Seq (SimpleRegex a) (SimpleRegex a) -- ^ Sequence (ab)
  deriving (Eq,Ord,Show)

-- | Converts from richer canonical regexes to minimal representation
toSimpleRegex :: SimpleRegex Char -> SimpleRegex Char
  -> SimpleRegex Char -> SimpleRegex Char -> SimpleRegex Char -> Abs.Reg
  -> SimpleRegex Char
toSimpleRegex any digit letter upper lower = helper
  where
    helper = \case
      Abs.RAlt l r -> Or (helper l) (helper r)
      Abs.RMinus l r -> Sub (helper l) (helper r)
      Abs.RSeq l r -> Seq (helper l) (helper r)
      Abs.RStar reg -> Rep (helper reg)
      Abs.RPlus reg -> let sreg = helper reg in sreg `Seq` Rep sreg
      Abs.ROpt reg -> Lambda `Or` helper reg
      Abs.REps -> Lambda
      Abs.RChar ch -> Term ch
      Abs.RAlts s -> charset s
      Abs.RSeqs s -> string s
      Abs.RDigit -> digit
      Abs.RLetter -> letter
      Abs.RUpper -> upper
      Abs.RLower -> lower
      Abs.RAny -> any

-- | Character set notation [asdf]
charset :: Ord a => [a] -> SimpleRegex a
charset = \case
  [] -> Phi
  chars -> foldr1 Or $ map Term chars

-- | Convenient seq notation "asdf"
string :: Ord a => [a] -> SimpleRegex a
string = \case
  [] -> Lambda
  chars -> foldr1 Seq $ map Term chars

-- | Used in place of actual regexes
type RegexID = Int

-- | A more convenient representation of regex trees
-- The empty language is represented as RegexNodeOr (IntSet.empty)
data RegexNode a
  = RegexNodeTerm a
  | RegexNodeEmpty
  | RegexNodeOr IntSet.IntSet
  | RegexNodeSeq RegexID RegexID
  | RegexNodeStar RegexID
  deriving (Ord, Eq, Show)

-- | SimpleRegex with precomputed possible starting characters and the value of delta
data AnnotatedRegexNode a = AnnotatedRegexNode
  { regexID :: RegexID
  , regexNode :: RegexNode a
  , regexStarts :: Set.Set a
  , regexContainsEmpty :: Bool
  }
  deriving Show

-- | Bidirectional mapping RegexID <-> the regex with precomputed values
data RegexTrees a = RegexTrees
  { tree2id :: Map.Map (RegexNode a) (AnnotatedRegexNode a)
  , id2tree :: IntMap.IntMap (AnnotatedRegexNode a)
  }
  deriving Show

emptyRegexTrees :: Ord a => RegexTrees a
emptyRegexTrees = RegexTrees
  { tree2id = Map.empty
  , id2tree = IntMap.empty
  }

-- | This throws on lookup failure because that should not happen if the
-- algorithm is correct
getByID :: RegexID -> RegexTrees a -> AnnotatedRegexNode a
getByID regID mp = case regID `IntMap.lookup` id2tree mp of
  Nothing -> error "RegexID not found in map"
  Just node -> node

-- | Insert a new node into the mapping
insertNode :: Ord a => AnnotatedRegexNode a -> RegexTrees a
  -> RegexTrees a
insertNode annot mp = RegexTrees
  { tree2id = Map.insert (regexNode annot) annot $ tree2id mp
  , id2tree = IntMap.insert (regexID annot) annot $ id2tree mp
  }

getByNode :: Ord a => RegexNode a -> RegexTrees a
  -> Maybe (AnnotatedRegexNode a)
getByNode node mp = node `Map.lookup` tree2id mp

-- | Creates a new node or returns an existing one if it is exactly the same
getOrNewID :: Ord a => RegexNode a -> Set.Set a -> Bool
  -> RegexTrees a -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewID node regexIDStarts regexIDContainsEmpty mp =
  case getByNode node mp of
    Just annot -> (mp, annot)
    Nothing -> let
        newID = IntMap.size $ id2tree mp
        annot = AnnotatedRegexNode
          { regexID = newID
          , regexNode = node
          , regexStarts = regexIDStarts
          , regexContainsEmpty = regexIDContainsEmpty}
      in (insertNode annot mp, annot)

-- | Returns the empty string regex (creates one if necessary)
getOrNewLambda :: Ord a => RegexTrees a
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewLambda = getOrNewID RegexNodeEmpty Set.empty True

-- | Returns the empty language regex (creates one if necessary)
getOrNewPhi :: Ord a => RegexTrees a
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewPhi = getOrNewID (RegexNodeOr IntSet.empty) Set.empty False

-- | unites nested 'Or's
-- if there is only one alternative, returns it unmodified
getOrNewOr :: Ord a => [AnnotatedRegexNode a] -> RegexTrees a
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewOr anns mp = let
    uniqAnns = IntMap.fromList $ map (\a -> (regexID a, a))
      $ concatMap listAnns anns
  in case IntMap.toList uniqAnns of
    [(_, ann)] -> (mp, ann)
    _ -> getOrNewID (RegexNodeOr $ IntMap.keysSet uniqAnns)
      (Set.unions $ map regexStarts $ IntMap.elems uniqAnns)
      (any regexContainsEmpty $ IntMap.elems uniqAnns) mp
  where
    -- listAnns :: AnnotatedRegexNode a -> [AnnotatedRegexNode a]
    listAnns ann =
      case regexNode $ getByID (regexID ann) mp of
        RegexNodeOr idset -> map (flip getByID mp) $ IntSet.toList idset
        _ -> [ann]

-- | normalizes s.t. left subtree is not a Seq;
-- removes Empty regexes;
-- Empty lang if left or right is empty lang
getOrNewSeq :: Ord a => AnnotatedRegexNode a -> AnnotatedRegexNode a
    -> RegexTrees a -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewSeq left right mp = let
    rightNode = regexNode right
  in if isPhi rightNode then (mp, right)
    else case rightNode of
      RegexNodeEmpty -> (mp, left)
      _ -> helper left right mp
  where
    -- | does not check right
    helper left right mp = let
        leftID = regexID left
        leftNode = regexNode left
      in if isPhi leftNode then (mp, left)
        else case leftNode of
          RegexNodeSeq llID lrID -> let
              ll = getByID llID mp
              lr = getByID lrID mp
              (mp1, mid) = helper lr right mp
            in helper ll mid mp1
          RegexNodeEmpty -> (mp, right)
          _ -> getOrNewID (RegexNodeSeq leftID (regexID right))
            (if regexContainsEmpty left
            then Set.union (regexStarts left) (regexStarts right)
            else regexStarts left)
            (regexContainsEmpty left && regexContainsEmpty right) mp
    isPhi = \case
      RegexNodeOr idset -> IntSet.null idset
      _ -> False

-- | returns a new or existing regex matching a single term (e.g. char)
getOrNewTerm :: Ord a => a -> RegexTrees a
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewTerm ch = getOrNewID (RegexNodeTerm ch) (Set.singleton ch) False

-- | Converts the simple regex into internal representation with precomputed
-- starts and delta
-- Converts subtractions
makeAnnotated :: Ord a => SimpleRegex a -> RegexTrees a
  -> (RegexTrees a, AnnotatedRegexNode a)
makeAnnotated reg mp = case reg of
  Term a -> getOrNewTerm a mp
  Lambda -> getOrNewLambda mp
  Phi -> getOrNewPhi mp
  Rep r -> let (newmp, rann) = makeAnnotated r mp in
    getOrNewID (RegexNodeStar (regexID rann)) (regexStarts rann) True newmp
  Or a b -> let
      rs = flattenOr a ++ flattenOr b
      (newmp, anns) = foldr (\r (mp, anns) ->
          let (newmp, ann) = makeAnnotated r mp in (newmp, ann:anns))
        (mp, []) rs
    in getOrNewOr anns newmp
  Sub a b -> let
      (mp1, nodeA) = makeAnnotated a mp
      (mp2, nodeB) = makeAnnotated b mp1
    in convertSub nodeA nodeB mp2
  Seq a b -> let
      (a', b') = reorderSeq a b
      (mp1, nodeA) = makeAnnotated a' mp
      (mp2, nodeB) = makeAnnotated b' mp1
    in getOrNewSeq nodeA nodeB mp2
  where
    flattenOr = \case
      Or a b -> flattenOr a ++ flattenOr b
      Phi -> []
      other -> [other]
    reorderSeq :: SimpleRegex a -> SimpleRegex a -> (SimpleRegex a, SimpleRegex a)
    reorderSeq a b = case a of
      Seq l r -> reorderSeq l (Seq r b)
      _ -> (a, b)

-- | Computes the derivative:
-- Da(a) = λ
-- Da(b) = φ
-- Da(λ) = Da(φ) = φ
-- Da(AB) = DaA|(δA)DaB
-- Da(A|B) = DaA|DaB
-- Da(A*) = (DaA)A*
derive :: Ord a => a -> AnnotatedRegexNode a -> RegexTrees a
  -> (RegexTrees a, AnnotatedRegexNode a)
derive ch reg mp = case regNode of
    RegexNodeTerm a -> if ch == a then getOrNewLambda mp else getOrNewPhi mp
    RegexNodeEmpty -> getOrNewPhi mp
    RegexNodeOr regs -> let
        (newmp, derivs) = IntSet.foldr (\regID (mp, derivs) ->
          let (mpnew, deriv) = derive ch (getByID regID mp) mp
          in (mpnew, deriv:derivs)) (mp, []) regs
      in getOrNewOr derivs newmp
    RegexNodeSeq leftID rightID -> let
        left = getByID leftID mp
        right = getByID rightID mp
        (mp1, derivLeft) = derive ch left mp
        (mp2, onlyLeftRes) = getOrNewSeq derivLeft right mp1
      in if regexContainsEmpty left
        then let (mp3, derivRight) = derive ch right mp2
          in getOrNewOr [onlyLeftRes, derivRight] mp3
        else (mp2, onlyLeftRes)
    RegexNodeStar aID -> let
        a = getByID aID mp
        (mp1, derivA) = derive ch a mp
      in getOrNewSeq derivA reg mp1
  where
    regNode = regexNode reg

-- | Converts (A-B) into an equivalent regex without subtraction (A and B do not
-- contain subtraction already)
-- For implementation details, see comment below
convertSub :: Ord a => AnnotatedRegexNode a -> AnnotatedRegexNode a
  -> RegexTrees a -> (RegexTrees a, AnnotatedRegexNode a)
convertSub a b mp = (\(mp, conv, _) -> (mp, conv)) $ helper a b mp 0 Map.empty
  where
    -- | returns:
    -- ( the updated map
    -- , the regex of all paths successfully converted
    -- , references to subtractions that reoccurred as their subexpressions
    --   (with the corresponding derivation sequences))
    helper :: Ord a => AnnotatedRegexNode a -> AnnotatedRegexNode a
      -> RegexTrees a
      -> Int -> Map.Map (RegexID, RegexID) Int
      -> ( RegexTrees a
         , AnnotatedRegexNode a
         , IntMap.IntMap (AnnotatedRegexNode a))

    -- | We derive 'a' and 'b' over each starting term of 'a' and 'b'.
    -- For each starting char 'startch':
    --   If only 'b' can start with 'startch', then 'startch' does not start the
    --     result.
    --   Else if only 'a' can start with 'startch', then this branch does not
    --     need subtraction. Add (startch)(derive startch 'a') to the result.
    --   Else if both 'a' and 'b' can start with 'startch':
    --     Da := derive startch a; Db := derive startch b
    --     -- note that a-b `contains` (startch)(Da - Db)
    --     (converted, looped) := Evaluate Da - Db
    --     Add to results: converted, looped
    --
    --   If we have not looped to this iteration:
    --     return (converted, looped)
    --   Else
    --     -- note that R=xR+y <=> R=x*y, where 'x' is a sequence of terms we
    --     -- derive over recursively
    --     return ((sequenceUpToLoop)* converted, looped - thisloop)
    helper a b mp depth prevStates = let
        (mp1, lambda) = getOrNewLambda mp
      in case currentState `Map.lookup` prevStates of
        Just prevdepth -> let
            (mp2, phi) = getOrNewPhi mp1
          in (mp2, phi, IntMap.singleton prevdepth lambda)
        Nothing -> let
            uniqLeftStarts = regexStarts a `Set.difference` regexStarts b
            sharedStarts = regexStarts a `Set.intersection` regexStarts b
            baseResList = if resDelta then [lambda] else []
            -- derive over terms not subtracted from 'a'
            (mp2, nonloopResList) = foldr (\startch (mp, reslist) -> let
                (mp', deriv) = derive startch a mp
                (mp'', term) = getOrNewTerm startch mp'
                (mp''', res) = getOrNewSeq term deriv mp''
              in (mp''', res:reslist)) (mp1, baseResList) uniqLeftStarts
            -- derive over terms that are subtracted, accumulate loops
            (mp3, resList, looped) = foldr
              (\startch (mp, reslist, loopedMap) ->
                let
                  (mpI, derivA) = derive startch a mp
                  (mpII, derivB) = derive startch b mpI
                  (mpIII, recConverted, recLooped) = helper derivA derivB mpII
                    (depth + 1) (Map.insert currentState depth prevStates)
                  (mpIV, term) = getOrNewTerm startch mpIII
                  (mpV, resConverted) = getOrNewSeq term recConverted mpIV
                  (mpVI, loopedMap') = foldr
                    (\(startDepth, reg) (mp, looped) ->
                      let (mp', reg') = getOrNewSeq term reg mp
                      in (mp', IntMap.insertWith (++)
                        startDepth [reg'] looped))
                    (mpV, loopedMap) $ IntMap.toList recLooped
                in (mpVI, resConverted:reslist, loopedMap'))
              (mp2, nonloopResList, IntMap.empty) sharedStarts
            -- merge loop sequences into 'RegexNodeOr's
            (mp4, mergedLoops) = foldr (\(startDepth, alts) (mp, tail) ->
                let (mp', merged) = getOrNewOr alts mp
                in (mp', (startDepth, merged):tail))
              (mp3, []) $ IntMap.toAscList looped
            mergedLoopsMap = IntMap.fromDistinctAscList mergedLoops
            -- converted regex
            (mp5, resOr) = getOrNewOr resList mp4
            myLoop = depth `IntMap.lookup` mergedLoopsMap
          in case myLoop of
            Nothing -> (mp5, resOr, mergedLoopsMap)
            Just preLoopSeq -> let
                (mp6, resSol) = equationSolutionRule preLoopSeq resOr mp5
              in (mp6, resSol, IntMap.deleteMax mergedLoopsMap)
      where
        currentState = (regexID a, regexID b)
        resDelta = regexContainsEmpty a && not (regexContainsEmpty b)
        -- | if not (containsEmpty preLoop), then
        -- R = (preLoop)R|(alt) iff R = (preLoop)*(alt)
        equationSolutionRule preLoop alt mp = let
            (mp1, preLoopStar) = getOrNewID (RegexNodeStar $ regexID preLoop)
              (regexStarts preLoop) True mp
          in getOrNewSeq preLoopStar alt mp1

-- | straightforward conversion
convertToSimpleRegex :: Ord a => RegexID -> RegexTrees a -> SimpleRegex a
convertToSimpleRegex regID mp = case node of
    RegexNodeEmpty -> Lambda
    RegexNodeTerm a -> Term a
    RegexNodeSeq leftID rightID -> Seq (convertToSimpleRegex leftID mp)
      (convertToSimpleRegex rightID mp)
    RegexNodeOr idset -> if IntSet.null idset then Phi else
      foldr1 Or (map (flip convertToSimpleRegex mp) $ IntSet.toList idset)
    RegexNodeStar rID -> Rep (convertToSimpleRegex rID mp)
  where
    reg = getByID regID mp
    node = regexNode reg

-- | Produces an equivalent regex without any 'Sub's
removeMinuses :: Ord a => SimpleRegex a -> SimpleRegex a
removeMinuses reg = convertToSimpleRegex (regexID annot) mp
  where
    (mp, annot) = makeAnnotated reg emptyRegexTrees

-- | visualizes the regex, showing the empty string as () and the empty language
-- as []. Uses parentheses to resolve precedence.
regexToString :: SimpleRegex Char -> String
regexToString = \case
  Term ch -> [ch]
  Lambda -> ""
  Phi -> "[]"
  Rep reg -> helper 5 reg ++ "*"
  Or l r -> helper 2 l ++ "|" ++ helper 2 r
  Sub l r -> helper 1 l ++ "-" ++ helper 2 r
  Seq l r -> helper 3 l ++ helper 3 r
  where
    helper :: Int -> SimpleRegex Char -> String
    helper prec reg
      | prec <= precedence reg = repr
      | otherwise = "(" ++ repr ++ ")"
      where repr = regexToString reg
    precedence = \case
      Term _ -> 5
      Lambda -> 0
      Phi -> 5
      Rep _ -> 4   -- 4 = Rep 5
      Or _ _ -> 2  -- 2 = Or 2 2
      Sub _ _ -> 1 -- 1 = Sub 1 2
      Seq _ _ -> 3 -- 3 = Seq 3 3
