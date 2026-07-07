{-|
  Module      : BNFC.RegexMinus
  Description : Converts away subtraction in regexes
  License     : Public Domain

  This module is intended to be imported qualified to use in lexer backends
  which wish to support general subtraction.

  Inspired by David J. Sankel's implementation.

  Many ideas taken from:

    - <http://home.chello.no/~mgrsby/sgmlintr/file0003.htm>
    - <http://home.chello.no/~mgrsby/sgmlintr/file0004.htm>
    - <http://home.chello.no/~mgrsby/sgmlintr/file0005.htm>
    - <https://arxiv.org/pdf/1502.03573>

  (accessible via the Wayback Machine).

  = EXPLANATION

  First, we define:

  - λ = the empty string "";
  - φ = the empty language [];
  - δR (for some regex R) = if λ∊R then λ else φ.
    For example, δ(a*b*) = λ; δ(ab*) = φ;
  - The derivative Dc(R) (for some character c and regex R) = a regex S such
    that S matches exactly the strings that are in R and start with @c@, but
    without the first character @c@.
    For example, Da((ab|cd)*) = b(ab|cd)*; Da(a*) = a*; Da(λ) = φ.

  Also note that:

  - λR = Rλ = R;
  - φR = Rφ = φ;
  - R|φ = R;
  - R-φ = R;
  - φ-R = φ;
  - Dc(R-S)=DcR-DcS.

  The main observation: if R contains strings that start with
  @a@, @b@, ..., @z@, then

                     R = a(DaR)|b(DbR)|...|z(DzR)|δR.

  Our strategy is to derive the subtraction until we arrive at a new subtraction
  that we have seen before, or eliminate the subtrahend.

  We obtain an FSA with states representing different subtractions and
  transitions being the characters we derive on. We add a fictitious final state
  and add transitions to it:
  - from all nullable states (where δR=λ) - a spontaneous transition λ;
  - from all states where a derivation on a character "c" eliminates the
    subtraction (Dc(R-S) = T) - a transition "cT".

  The obtained FSA is converted to a regex using state elimination
  (<https://arxiv.org/pdf/1502.03573>, section 3.2).

  Let's consider an example: we have to convert (ab|ac)*-(ac)* to an equivalent
  regex without the subtraction operator.

  We name the initial state (R).

  > Da(R)  = (b|c)(ab|ac)* - c(ac)* = (new state S1)
  > Db(S1) = (ab|ac)*  (subtraction eliminated)
  > Dc(S1) = (ab|ac)* - (ac)* = (loop to R)

  Since δR=δS1=φ, we have no spontaneous transitions to the final state.
  The FSA:

     (---) ----a----> (----)                  (-----)
  -->( R )            ( S1 ) ---b(ab|ac)*---> ( Fin )
     (___) <----c---- (____)                  (_____)

  We eliminate S1 by adding an R->R transition "aφ*c" = "ac" and an R->Fin
  transition "aφ*b(ab|ac)*" = "ab(ab|ac)*":

  -->(---) ---ab(ab|ac)*-->(-----)
     ( R )                 ( Fin )
   +-(___)<-+              (_____)
   |        |
   +---ac --+

  We eliminate R and obtain (ac)*ab(ab|ac)*.

  This implementation assigns a unique ID to every regex and uses the IDs to
  compare states.
-}

module BNFC.RegexMinus
  (
    -- * The 'SimpleRegex' type and its smart constructors
    SimpleRegex(..)
  , charset
  , string

    -- * 'SimpleRegex' conversions
  , toSimpleRegex
  , regexToString
  , simplify

    -- * 'SimpleRegex' transformations
  , removeMinuses
  ) where

-- Data types
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

-- Interaction with the Reg type
import qualified BNFC.Abs as Abs

-- | Describes a regular expression.
-- Purposefully minimalistic for simpler processing.
data SimpleRegex a
  = Term a  -- ^ Matches one character
  | Lambda  -- ^ This is the 0-length string
  | Phi     -- ^ Recognizes no strings
  | Rep (SimpleRegex a)  -- ^ Kleene Star (*)
  | Or (SimpleRegex a) (SimpleRegex a)   -- ^ Or (|)
  | Sub (SimpleRegex a) (SimpleRegex a)  -- ^ Regex Subtraction (-)
  | Seq (SimpleRegex a) (SimpleRegex a)  -- ^ Sequence (ab)
  deriving (Eq, Ord, Show)

-- | Converts from richer canonical 'Abs.Reg's
-- to the 'SimpleRegex' representation.
toSimpleRegex :: Ord a =>
     (Char -> SimpleRegex a)  -- ^ converts a single character.
  -> SimpleRegex a  -- ^ all Unicode characters.
  -> SimpleRegex a  -- ^ all digits.
  -> SimpleRegex a  -- ^ all isolatin1 letters.
  -> SimpleRegex a  -- ^ all uppercase isolatin1 letters.
  -> SimpleRegex a  -- ^ all lowercase isolatin1 letters.
  -> Abs.Reg        -- ^ the 'Abs.Reg' to simplify into a 'SimpleRegex'.
  -> SimpleRegex a
toSimpleRegex fromChar any digit letter upper lower = helper
  where
    helper = \case
      Abs.RAlt l r   -> Or (helper l) (helper r)
      Abs.RMinus l r -> Sub (helper l) (helper r)
      Abs.RSeq l r   -> Seq (helper l) (helper r)
      Abs.RStar reg  -> Rep (helper reg)
      Abs.RPlus reg  -> let sreg = helper reg in sreg `Seq` Rep sreg
      Abs.ROpt reg   -> Lambda `Or` helper reg
      Abs.REps       -> Lambda
      Abs.RChar ch   -> fromChar ch
      Abs.RAlts s    -> foldr Or Phi $ map fromChar s
      Abs.RSeqs s    -> foldr Seq Lambda $ map fromChar s
      Abs.RDigit     -> digit
      Abs.RLetter    -> letter
      Abs.RUpper     -> upper
      Abs.RLower     -> lower
      Abs.RAny       -> any

-- | Returns a 'SimpleRegex' that matches any one of the items in the list.
charset :: Ord a => [a] -> SimpleRegex a
charset = \case
  []    -> Phi
  chars -> foldr1 Or $ map Term chars

-- | Returns a 'SimpleRegex' that matches the exact sequence of items as in the
-- list.
string :: Ord a => [a] -> SimpleRegex a
string = \case
  []    -> Lambda
  chars -> foldr1 Seq $ map Term chars

-- | Used in place of actual regexes. Makes equality checking doable in /O(1)/.
newtype RegexID = RegexID Int
  deriving (Ord, Eq, Show)

-- | Extracts the @Int@ representation of the t'RegexID'.
-- Used with @IntMap@s and @IntSet@s.
regexID2Int :: RegexID -> Int
regexID2Int (RegexID i) = i

-- | A more convenient representation of regex trees.
-- The empty language is represented as @v'RegexNodeOr' IntSet.empty@.
data RegexNode a
  = RegexNodeTerm !a                -- ^ Matches the single element @a@.
  | RegexNodeEmpty                  -- ^ Matches the empty string.
  | RegexNodeOr !IntSet             -- ^ Union of regexes with IDs in the set.
  | RegexNodeSeq !RegexID !RegexID  -- ^ Sequence of regexes with given IDs.
  | RegexNodeStar !RegexID          -- ^ Kleene star of the regex with given ID.
  deriving (Ord, Eq)

-- | 'SimpleRegex' with precomputed possible starting characters nullability
data AnnotatedRegexNode a = AnnotatedRegexNode
  { regexID     :: !RegexID        -- ^ The ID.
  , regexNode   :: !(RegexNode a)  -- ^ The regex tree node.
  , regexStarts :: !(Set a)
    -- ^ all characters/elements that start a string matching the regex.
  , regexContainsEmpty :: !Bool
    -- ^ Does the empty string match this regex?
  }

-- | Bidirectional mapping:
-- t'RegexID' <-> the t'AnnotatedRegexNode' with precomputed values
data RegexTrees a = RegexTrees
  { tree2id :: (Map (RegexNode a) (AnnotatedRegexNode a))
    -- ^ Maps the tree node to its ID.
  , id2tree :: (IntMap (AnnotatedRegexNode a))
    -- ^ Maps the node ID to the tree node.
  }

-- | Returns the empty t'RegexTrees' data structure.
emptyRegexTrees :: Ord a => RegexTrees a
emptyRegexTrees = RegexTrees
  { tree2id = Map.empty
  , id2tree = IntMap.empty
  }

-- | Returns the information about the regex tree node by its ID.
-- This throws on lookup failure because if the algorithm is correct then
-- no lookup should fail.
getByID :: RegexID -> RegexTrees a -> AnnotatedRegexNode a
getByID (RegexID regID) mp = case regID `IntMap.lookup` id2tree mp of
  Nothing   -> error "RegexID not found in map"
  Just node -> node

-- | Insert a new node into the mapping.
-- Assumes that such an (ID, node) combination has not been inserted before.
-- Constructing the t'AnnotatedRegexNode' is the user's responsibility for
-- performance reasons.
insertNode :: Ord a =>
     AnnotatedRegexNode a
  -> RegexTrees a
  -> RegexTrees a
insertNode annot mp = RegexTrees
  { tree2id = Map.insert (regexNode annot) annot $ tree2id mp
  , id2tree = IntMap.insert (regexID2Int $ regexID annot) annot $ id2tree mp
  }

-- | Looks up the regex node in the map.
getByNode :: Ord a =>
     RegexNode a
  -> RegexTrees a
  -> Maybe (AnnotatedRegexNode a)
getByNode node mp = node `Map.lookup` tree2id mp

-- | Creates a new node or returns an existing one if it is exactly the same.
getOrNewID :: Ord a =>
     RegexNode a   -- ^ The contents of the regex node.
  -> Set a         -- ^ Start characters the regex accepts.
  -> Bool          -- ^ Does the regex accept the empty string?
  -> RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewID node regexIDStarts regexIDContainsEmpty mp =
  case getByNode node mp of
    Just annot -> (mp, annot)
    Nothing    -> let
        newID = IntMap.size $ id2tree mp
        annot = AnnotatedRegexNode
          { regexID            = RegexID newID
          , regexNode          = node
          , regexStarts        = regexIDStarts
          , regexContainsEmpty = regexIDContainsEmpty}
      in (insertNode annot mp, annot)

-- | Returns the empty string regex (creates one if none exists yet).
getOrNewLambda :: Ord a =>
     RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewLambda = getOrNewID RegexNodeEmpty Set.empty True

-- | Returns the empty language regex (creates one if none exists yet).
getOrNewPhi :: Ord a =>
     RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewPhi = getOrNewID (RegexNodeOr IntSet.empty) Set.empty False

-- | Returns a new regex that is equivalent to the union of the given regexes.
-- Unites nested v'RegexNodeOr's; if only one regex is given,
-- returns it unmodified. If an exact copy of the new v'RegexNodeOr' has already
-- been created, returns that one.
getOrNewOr :: Ord a =>
     [AnnotatedRegexNode a]  -- ^ Alternatives to unite.
  -> RegexTrees a            -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewOr anns mp = let
    uniqAnns = IntMap.fromList $ map (\ a -> (regexID2Int $ regexID a, a))
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
        RegexNodeOr idset -> map (flip getByID mp . RegexID)
          $ IntSet.toList idset
        _                 -> [ann]

-- | Returns a new regex that is a sequence of the given two.
-- Normalizes such that the left regex is not a v'RegexNodeSeq' itself
-- (maintains right-associativity). This is done so that removing one term from
-- the left is fast.
-- Removes v'RegexNodeEmpty' regexes; returns an empty language if the left or
-- the right argument is an empty language.
-- If an exact copy of the new v'RegexNodeSeq' has already been created,
-- returns that one.
getOrNewSeq :: Ord a =>
     AnnotatedRegexNode a  -- ^ The left node.
  -> AnnotatedRegexNode a  -- ^ The right node.
  -> RegexTrees a          -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewSeq left right mp = let rightNode = regexNode right in
    if isPhi rightNode
    then (mp, right)
    else case rightNode of
      RegexNodeEmpty -> (mp, left)
      _              -> helper left right mp
  where
    -- | Does not check @right@.
    helper left right mp = let
        leftID   = regexID left
        leftNode = regexNode left
      in
        if isPhi leftNode
        then (mp, left)
        else case leftNode of
          RegexNodeSeq llID lrID -> let
              ll = getByID llID mp
              lr = getByID lrID mp
              (mp1, mid) = helper lr right mp
            in helper ll mid mp1
          RegexNodeEmpty         -> (mp, right)
          _                      -> getOrNewID
            (RegexNodeSeq leftID (regexID right))
            (if regexContainsEmpty left
              then Set.union (regexStarts left) (regexStarts right)
              else regexStarts left)
            (regexContainsEmpty left && regexContainsEmpty right) mp
    -- | Is the regex an empty language?
    isPhi = \case
      RegexNodeOr idset -> IntSet.null idset
      _                 -> False

-- | Returns a new or existing regex matching a single term (e.g. @Char@).
getOrNewTerm :: Ord a =>
     a             -- ^ The singular regex term (@Char@, @Int8@...).
  -> RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewTerm ch = getOrNewID (RegexNodeTerm ch) (Set.singleton ch) False

-- | Converts the simple regex into an internal representation with precomputed
-- starts and nullability.
-- Converts away subtractions.
makeAnnotated :: Ord a =>
     SimpleRegex a
     -- ^ The regular expression to convert (could contain subtractions).
  -> RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
makeAnnotated reg mp = case reg of
  Term a -> getOrNewTerm a mp
  Lambda -> getOrNewLambda mp
  Phi    -> getOrNewPhi mp
  Rep r  -> let (newmp, rann) = makeAnnotated r mp in
    getOrNewID (RegexNodeStar (regexID rann)) (regexStarts rann) True newmp
  Or a b -> let
      rs = flattenOr a ++ flattenOr b
      (newmp, anns) =
        foldr (\ r (mp, anns) ->
          let (newmp, ann) = makeAnnotated r mp in (newmp, ann : anns))
        (mp, []) rs
    in getOrNewOr anns newmp
  Sub a b -> let
      (mp1, nodeA) = makeAnnotated a mp
      (mp2, nodeB) = makeAnnotated b mp1
    in convertSub nodeA nodeB mp2
  Seq a b -> let
      (a', b')     = reorderSeq a b
      (mp1, nodeA) = makeAnnotated a' mp
      (mp2, nodeB) = makeAnnotated b' mp1
    in getOrNewSeq nodeA nodeB mp2
  where
    flattenOr = \case
      Or a b -> flattenOr a ++ flattenOr b
      Phi    -> []
      other  -> [other]
    -- | Maintains right-associativity of v'Seq'.
    reorderSeq ::
         SimpleRegex a
      -> SimpleRegex a
      -> (SimpleRegex a, SimpleRegex a)
    reorderSeq a b = case a of
      Seq l r -> reorderSeq l (Seq r b)
      _       -> (a, b)

-- | Computes the derivative:
-- Da(a)   = λ
-- Da(b)   = φ
-- Da(λ)   = φ
-- Da(φ)   = φ
-- Da(AB)  = DaA|(δA)DaB
-- Da(A|B) = DaA|DaB
-- Da(A*)  = (DaA)A*
derive :: Ord a =>
     a                     -- ^ The starting character.
  -> AnnotatedRegexNode a  -- ^ The regex to derive.
  -> RegexTrees a          -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
derive ch reg mp = case regNode of
    RegexNodeTerm a ->
      if ch == a
      then getOrNewLambda mp
      else getOrNewPhi mp
    RegexNodeEmpty   -> getOrNewPhi mp
    RegexNodeOr regs -> let
        (newmp, derivs) = IntSet.foldr (\ regID (mp, derivs) ->
          let (mpnew, deriv) = derive ch (getByID (RegexID regID) mp) mp
          in (mpnew, deriv:derivs)) (mp, []) regs
      in getOrNewOr derivs newmp
    RegexNodeSeq leftID rightID -> let
        left               = getByID leftID mp
        right              = getByID rightID mp
        (mp1, derivLeft)   = derive ch left mp
        (mp2, onlyLeftRes) = getOrNewSeq derivLeft right mp1
      in
        if regexContainsEmpty left
        then
          let (mp3, derivRight) = derive ch right mp2
          in getOrNewOr [onlyLeftRes, derivRight] mp3
        else (mp2, onlyLeftRes)
    RegexNodeStar aID -> let
        a             = getByID aID mp
        (mp1, derivA) = derive ch a mp
      in getOrNewSeq derivA reg mp1
  where
    regNode = regexNode reg

-- | Produces an equivalent regex without any v'Sub's
removeMinuses :: Ord a => SimpleRegex a -> SimpleRegex a
removeMinuses reg = convertToSimpleRegex (regexID annot) mp
  where
    (mp, annot) = makeAnnotated reg emptyRegexTrees

-- | Converts (A-B) into an equivalent regex without subtraction (A and B do not
-- contain subtraction already).
convertSub :: Ord a =>
     AnnotatedRegexNode a  -- ^ The minuend.
  -> AnnotatedRegexNode a  -- ^ The subtrahend.
  -> RegexTrees a          -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
convertSub a b mp = (\ (mp, conv, _) -> (mp, conv)) $ helper a b mp 0 Map.empty
  where
    -- | returns:
    -- ( the updated map
    -- , the regex of all paths successfully converted
    -- , references to subtractions that reoccurred as their subexpressions
    --   (with the corresponding derivation sequences))
    helper :: Ord a =>
         AnnotatedRegexNode a  -- ^ The minuend.
      -> AnnotatedRegexNode a  -- ^ The subtrahend.
      -> RegexTrees a          -- ^ The current regex collection.
      -> Int
        -- ^ The current depth of recursion (= derivation count).
      -> Map (RegexID, RegexID) Int
        -- ^ The previous subtractions and their depths of recursion.
      -> ( RegexTrees a
         , AnnotatedRegexNode a
         , IntMap (AnnotatedRegexNode a))

    -- We derive 'a' and 'b' over each starting term of 'a' and 'b'.
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
    helper a b mp depth prevStates =
      let (mp1, lambda) = getOrNewLambda mp
      in case currentState `Map.lookup` prevStates of
        Just prevdepth ->
          let (mp2, phi) = getOrNewPhi mp1
          in (mp2, phi, IntMap.singleton prevdepth lambda)
        Nothing -> let
            uniqLeftStarts = regexStarts a `Set.difference` regexStarts b
            sharedStarts = regexStarts a `Set.intersection` regexStarts b
            baseResList = if resDelta then [lambda] else []
            -- derive over terms not subtracted from 'a'
            (mp2, nonloopResList) = foldr (\ startch (mp, reslist) -> let
                (mp', deriv) = derive startch a mp
                (mp'', term) = getOrNewTerm startch mp'
                (mp''', res) = getOrNewSeq term deriv mp''
              in (mp''', res : reslist)) (mp1, baseResList) uniqLeftStarts
            -- derive over terms that are subtracted, accumulate loops
            (mp3, resList, looped) = foldr
              (\ startch (mp, reslist, loopedMap) -> let
                  (mpI, derivA) = derive startch a mp
                  (mpII, derivB) = derive startch b mpI
                  (mpIII, recConverted, recLooped) = helper derivA derivB mpII
                    (depth + 1) (Map.insert currentState depth prevStates)
                  (mpIV, term) = getOrNewTerm startch mpIII
                  (mpV, resConverted) = getOrNewSeq term recConverted mpIV
                  (mpVI, loopedMap') = foldr
                    (\ (startDepth, reg) (mp, looped) ->
                      let (mp', reg') = getOrNewSeq term reg mp
                      in (mp', IntMap.insertWith (++)
                        startDepth [reg'] looped))
                    (mpV, loopedMap) $ IntMap.toList recLooped
                in (mpVI, resConverted : reslist, loopedMap'))
              (mp2, nonloopResList, IntMap.empty) sharedStarts
            -- merge loop sequences into 'RegexNodeOr's
            (mp4, mergedLoops) = foldr (\ (startDepth, alts) (mp, tail) ->
                let (mp', merged) = getOrNewOr alts mp
                in (mp', (startDepth, merged) : tail))
              (mp3, []) $ IntMap.toAscList looped
            mergedLoopsMap = IntMap.fromDistinctAscList mergedLoops
            -- converted regex
            (mp5, resOr) = getOrNewOr resList mp4
            myLoop = depth `IntMap.lookup` mergedLoopsMap
          in case myLoop of
            Nothing -> (mp5, resOr, mergedLoopsMap)
            Just preLoopSeq ->
              let (mp6, resSol) = equationSolutionRule preLoopSeq resOr mp5
              in (mp6, resSol, IntMap.deleteMax mergedLoopsMap)
      where
        currentState = (regexID a, regexID b)
        resDelta = regexContainsEmpty a && not (regexContainsEmpty b)
        -- | if not (containsEmpty preLoop), then
        -- R = (preLoop)R|(alt) iff R = (preLoop)*(alt)
        equationSolutionRule preLoop alt mp =
          let (mp1, preLoopStar) = getOrNewID (RegexNodeStar $ regexID preLoop)
                (regexStarts preLoop) True mp
          in getOrNewSeq preLoopStar alt mp1

data FSA a = FSA
  { fsa_transitions :: IntMap (IntMap (SimpleRegex a))
  , fsa_revEdges    :: IntMap (IntSet)
  }

fsaEmpty :: Ord a => FSA a
fsaEmpty = FSA
  { fsa_transitions = IntMap.empty
  , fsa_revEdges    = IntMap.empty
  }

fsaAddVertex :: Ord a => FSA a -> (FSA a, Int)
fsaAddVertex FSA
  { fsa_transitions = trans
  , fsa_revEdges    = rev
  } = (FSA
    { fsa_transitions = IntMap.insert n IntMap.empty trans
    , fsa_revEdges    = IntMap.insert n IntSet.empty rev
    }, n)
  where
    n = IntMap.size trans

fsaAddTransition :: Ord a => Int -> Int -> SimpleRegex a -> FSA a -> FSA a
fsaAddTransition from to how (FSA
  { fsa_transitions = trans
  , fsa_revEdges    = rev
  }) = FSA
  { fsa_transitions =
      IntMap.update (Just . IntMap.insertWith Or to how) from trans
  , fsa_revEdges =
      if from /= to
      then IntMap.update (Just . IntSet.insert from) to rev
      else rev
  }

fsaPop :: Ord a
  => FSA a
  -> (FSA a, Int, IntMap (SimpleRegex a), IntSet)
fsaPop FSA
  { fsa_transitions = trans
  , fsa_revEdges    = rev
  } =
  ( FSA
    { fsa_transitions = IntMap.map (IntMap.delete index) trans
    , fsa_revEdges    = IntMap.map (IntSet.delete index) rev
    }
  , index
  , mytrans
  , myRevEdges
  )
  where
    ((index, mytrans),    _) = IntMap.deleteFindMax trans
    ((_,     myRevEdges), _) = IntMap.deleteFindMax rev

fsaEliminate :: Ord a => FSA a -> FSA a
fsaEliminate fsa =
  foldr (\ (u, utrans, v, vtrans) ->
      fsaAddTransition u v $ utrans `mergeTrans` vtrans)
    fsaPopped
    [ let
        Just fromU  = IntMap.lookup u (fsa_transitions fsa)
        Just utrans = IntMap.lookup index fromU
      in (u, utrans, v, vtrans)
    | u <- IntSet.toList myrev, (v, vtrans) <- IntMap.toList transToOthers
    ]
  where
    (fsaPopped, index, mytrans, myrev) = fsaPop fsa
    (transToOthers, mergeTrans) = case index `IntMap.lookup` mytrans of
      Nothing -> (mytrans, Seq)
      Just r  ->
        ( IntMap.delete index mytrans
        , (\ from to -> from `Seq` (Rep r `Seq` to))
        )

-- | Converts an internal 'RegexNode' (represented with a t'RegexID')
-- into a 'SimpleRegex'.
convertToSimpleRegex :: Ord a =>
     RegexID       -- ^ The regex node to convert.
  -> RegexTrees a  -- ^ The current regex collection.
  -> SimpleRegex a
convertToSimpleRegex regID mp = case node of
    RegexNodeEmpty  -> Lambda
    RegexNodeTerm a -> Term a
    RegexNodeSeq leftID rightID ->
      convertToSimpleRegex leftID mp `Seq` convertToSimpleRegex rightID mp
    RegexNodeOr idset ->
      if IntSet.null idset
      then Phi
      else foldr1 Or
        (map (flip convertToSimpleRegex mp . RegexID)
        $ IntSet.toList idset)
    RegexNodeStar rID -> Rep (convertToSimpleRegex rID mp)
  where
    reg  = getByID regID mp
    node = regexNode reg

simplify :: Ord a => SimpleRegex a -> SimpleRegex a
simplify reg =
  let (mp, annot) = makeAnnotated reg emptyRegexTrees
  in convertToSimpleRegex (regexID annot) mp

-- | Visualizes the regex, showing the empty string as () and the empty language
-- as []. Uses parentheses to resolve precedence.
regexToString :: SimpleRegex Char -> String
regexToString = \case
  Term ch -> [ch]
  Lambda  -> ""
  Phi     -> "[]"
  Rep reg -> helper 5 reg ++ "*"
  Or l r  -> helper 2 l ++ "|" ++ helper 2 r
  Sub l r -> helper 1 l ++ "-" ++ helper 2 r
  Seq l r -> helper 3 l ++ helper 3 r
  where
    helper :: Int -> SimpleRegex Char -> String
    helper prec reg
      | prec <= precedence reg = repr
      | otherwise = "(" ++ repr ++ ")"
      where repr = regexToString reg
    precedence = \case
      Term  _ -> 5
      Lambda  -> 0
      Phi     -> 5
      Rep _   -> 4  -- Rep 5
      Or  _ _ -> 2  -- Or 2 2
      Sub _ _ -> 1  -- Sub 1 2
      Seq _ _ -> 3  -- Seq 3 3
