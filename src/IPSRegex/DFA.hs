module IPSRegex.DFA (minimiseDfa, nfaToDfa, runDfa, DFA (..)) where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.State (
  MonadState (get),
  evalState,
  gets,
  modify',
 )
import Data.Bifunctor (first, second)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as Sq
import Data.Set (Set, disjoint, intersection, isSubsetOf, (\\))
import qualified Data.Set as Set
import IPSRegex.NFA (
  Alphabet,
  NFA (..),
  NFAState,
  NFAStateTransitions,
  NFATransitions,
  TransSymbol (..),
 )

-- | During conversion from an NFA to a DFA, each DFA state is represented as
-- a set of NFA states to make the subset construction easier to implement.
type DFAState' = Set NFAState

-- | Transitions from a single DFA state during conversion.
type DFAStateTransitions' = Map Char DFAState'

-- | The outgoing transitions for the states in the DFA during conversion.
type DFATransitions' = Map DFAState' DFAStateTransitions'

-- | After conversion, each DFA state is represented by a unique integer.
type DFAState = Int

-- | Transitions from a single DFA state after conversion.
type DFAStateTransitions = Map Char DFAState

-- | The outgoing transitions for the states in the DFA after conversion.
type DFATransitions = Map DFAState DFAStateTransitions

-- | Information about a deterministic finite automaton.
data DFA = DFA
  { dfaStates :: Set DFAState
  , dfaStartState :: DFAState
  , dfaAcceptingStates :: Set DFAState
  , dfaTransitions :: DFATransitions
  , dfaAlphabet :: Set Char
  }

-- | Find the ε-closure of a set of states @m@.
-- The ε-closure of a set of states M is M extended with all states
-- that can be reached from any state in M using any number of
-- ε-transitions.
epsClosure :: NFATransitions -> Set NFAState -> Set NFAState
epsClosure trns m = epsClosure' (Set.elems m) m
  where
    epsClosure' [] clsr = clsr
    epsClosure' (x : unmarked) clsr =
      let fMx = f_M x
          newClsr = clsr <> fMx
          newUnmarked = Set.elems $ fMx \\ clsr
       in epsClosure' (newUnmarked ++ unmarked) newClsr

    f_M x = Set.insert x $ epsTransitions trns (Set.singleton x)

-- | Determine which states can be reached from @states@ via a single
-- ε-transition.
epsTransitions :: NFATransitions -> Set NFAState -> Set NFAState
epsTransitions trns states = Set.unions $ Set.map reachableViaEps states
  where
    reachableViaEps s =
      M.findWithDefault Set.empty Epsilon . M.findWithDefault M.empty s $ trns

-- | Convert an NFA to a DFA, first by using the subset construction algorithm
-- described in section 1.5.2 in 'Introduction to Compiler Design' to construct
-- an equivalent DFA, then minimising this DFA.
nfaToDfa :: NFA -> DFA
nfaToDfa nfa = minimiseDfa dfa
  where
    nfaTrns = nfaTransitions nfa
    dfaAlph = nfaAlphabet nfa
    startStateS = Set.singleton $ nfaStartState nfa
    s0' = epsClosure nfaTrns startStateS
    (states, allTrns) = subsetConstruction s0' nfaTrns dfaAlph

    allStates = Set.insert s0' states

    nfaAcc = nfaAcceptingState nfa

    accepting = Set.filter (Set.member nfaAcc) allStates

    stateIdMap = M.fromAscList $ zip (Set.toAscList allStates) [0 ..]
    converter = (M.!) stateIdMap
    idsAllTrns = convertTrns converter allTrns
    idsAllStates = Set.fromList $ M.elems stateIdMap
    idS0' = converter s0'
    idsAccepting = Set.map converter accepting

    dfa =
      DFA
        { dfaStates = idsAllStates
        , dfaStartState = idS0'
        , dfaAcceptingStates = idsAccepting
        , dfaTransitions = idsAllTrns
        , dfaAlphabet = dfaAlph
        }

-- | Convert a map of transitions such that all keys in the outer map and
-- all values in the inner map are now integers instead of sets.
convertTrns ::
  (DFAState' -> DFAState) ->
  DFATransitions' ->
  DFATransitions
-- TODO: Maybe just move this up inside 'nfaToDfa'.
convertTrns converter trns' = trnsWIds
  where
    idKeysMap = M.mapKeys converter trns'
    trnsWIds = M.map (M.map converter) idKeysMap

-- | The subset construction algorithm for converting an NFA to a DFA.
-- Equivalent to Algorithm 1.3 (page 16) in 'Introduction to Compiler Design'.
-- It uses the work-list algorithm "template" for efficiency.
subsetConstruction ::
  DFAState' ->
  NFATransitions ->
  Alphabet ->
  (Set DFAState', DFATransitions')
subsetConstruction s0' nfaTrns nfaAlph =
  subsetConstruction' [s0'] Set.empty M.empty
  where
    subsetConstruction' [] states trns = (states, trns)
    subsetConstruction' (s' : unmarked) states trns =
      let (states', trns') = moveAll s'
          newStates = states <> states'
          newUnmarked = Set.elems $ states' \\ states
          newTrns = trns `combine` trns'
       in subsetConstruction' (newUnmarked ++ unmarked) newStates newTrns

    -- Make a transition from one DFA state to another via c.
    connect from c to = M.singleton from (M.singleton c to)

    -- Combine/union two 'DFATransitions'-mappings.
    combine = M.unionWith M.union

    -- For each character c in the original NFA's alphabet, find which state
    -- s' should transition to via c.
    moveAll s' =
      foldr (findStatesAndTransitions s') (Set.empty, M.empty) nfaAlph

    findStatesAndTransitions s' c acc@(states, trns) =
      let to = move s' c
       in if null to
            then acc
            else (Set.insert to states, trns `combine` connect s' c to)

    -- Calculate the (DFA) state that s' should reach when transitioning via c.
    move s' c = epsClosure nfaTrns . Set.unions $ Set.map (reachableVia c) s'

    -- Find every (NFA) state reachable from s via c.
    reachableVia c s =
      M.findWithDefault Set.empty (Symbol c) $
        M.findWithDefault M.empty s nfaTrns

-- | Minimise a DFA by combining equivalent states.
--
-- Precondition: Either (1) @dfa@'s move function is total, i.e., there are no
-- undefined transitions, or (2) @dfa@ contains no dead states. This is
-- satisfied as long as the original regex is converted to an NFA with
-- @regexpToNfa@ and this NFA is converted to a DFA with @nfaToDfa@.
minimiseDfa :: DFA -> DFA
minimiseDfa dfa = minimalDfa
  where
    states = dfaStates dfa
    accepting = dfaAcceptingStates dfa
    nonAccepting = states \\ accepting
    trns = dfaTransitions dfa
    alph = dfaAlphabet dfa
    partition = Sq.fromList [accepting, nonAccepting]
    equivalentStates = hopcroft trns states alph partition partition

    idEquivStatePairs = zip [0 ..] equivalentStates

    findNewId [] s = error "s couldn't be found"
    findNewId ((newId, states) : rest) s
      | s `Set.member` states = (s, newId)
      | otherwise = findNewId rest s

    newStateIdsMap =
      M.fromAscList $
        Set.toAscList $
          Set.map (findNewId idEquivStatePairs) states

    getNewId = (M.!) newStateIdsMap
    trnsMinimal = M.map (M.map getNewId) . M.mapKeys getNewId $ trns
    statesMinimal = Set.fromList $ M.elems newStateIdsMap
    startStateMinimal = getNewId $ dfaStartState dfa
    acceptingMinimal = Set.map getNewId accepting

    minimalDfa =
      DFA
        { dfaStartState = startStateMinimal
        , dfaStates = statesMinimal
        , dfaAcceptingStates = acceptingMinimal
        , dfaTransitions = trnsMinimal
        , dfaAlphabet = alph
        }

-- | Hopcroft's algorithm for DFA minimisation. Based on pseudocode from
-- Wikipedia's 'DFA minimization' page.
hopcroft ::
  DFATransitions ->
  Set DFAState ->
  Alphabet ->
  Seq (Set DFAState) ->
  Seq (Set DFAState) ->
  [Set DFAState]
hopcroft dfaTrns allStates alph waiting partition =
  toList $ evalState hopcroftM (waiting, partition)
  where
    alphList = Set.elems alph

    -- Check if 's' can reach 'a' via the character 'c'.
    -- OPTIM: Could construct an inverse transition map beforehand.
    canReachVia c a s =
      case M.lookup s dfaTrns of
        Nothing -> False
        Just sTrns -> maybe False (`Set.member` a) $ M.lookup c sTrns

    nonEmptyIntersectAndDiff x y = not $ disjoint x y || y `isSubsetOf` x

    -- If Y is in W, replace Y with X ∩ Y and Y - X in W.
    -- Otherwise, insert the smaller of X ∩ Y and Y - X.
    modifyWaiting mbI intersect diff = do
      waiting <- gets fst
      let newWaiting =
            case mbI of
              Nothing ->
                if Set.size intersect <= Set.size diff
                  then intersect <| waiting
                  else diff <| waiting
              Just i -> Sq.fromList [intersect, diff] >< Sq.deleteAt i waiting
      modify' (first $ const newWaiting)

    -- Replace Y with X ∩ Y and Y - X in P.
    modifyPartition i intersect diff = do
      partition <- gets snd
      let yRemoved = Sq.deleteAt i partition
          newPartition = Sq.fromList [intersect, diff] >< yRemoved
      modify' (second $ const newPartition)

    -- Handle a Y for which X ∩ Y ≠ ∅ and Y - X ≠ ∅.
    handleY x y = do
      (waiting, partition) <- get
      let intersect = x `intersection` y
          diff = y \\ x
          p_i = fromMaybe (error "shouldn't happen") $ Sq.elemIndexL y partition
          m_w_i = Sq.elemIndexL y waiting
      modifyPartition p_i intersect diff
      modifyWaiting m_w_i intersect diff

    -- Handle a character from the original DFA's alphabet.
    handleChar a c = do
      partition <- gets snd
      let x = Set.filter (canReachVia c a) allStates
          allYs = Sq.filter (nonEmptyIntersectAndDiff x) partition
      forM_ allYs (handleY x)

    -- Handle a set A from W.
    handleA a = forM_ alphList (handleChar a) >> hopcroftM

    hopcroftM = do
      waiting <- gets fst
      case waiting of
        Empty -> gets snd
        (a :<| waiting') ->
          -- Remove A from W before continuing.
          modify' (first $ const waiting') >> handleA a

-- This was also inspired by Troels Henriksens 'runDFA' function
-- in 'https://sigkill.dk/hacks/scripts/HsGrep.hs'.
runDfa :: String -> DFA -> Maybe (String, String)
runDfa str' dfa = runDfa' str' (dfaStartState dfa)
  where
    dfaTrns = dfaTransitions dfa
    accepting = dfaAcceptingStates dfa

    isAccepting st = st `Set.member` accepting

    runDfa' str currState
      | isAccepting currState =
          -- If the current state is accepting, we still try to consume the
          -- next character(s), but if this fails, we stop here.
          consume str currState <|> Just ("", str)
      | otherwise = consume str currState

    consume [] _ = Nothing
    consume (c : cs) currState = do
      stateTrns <- M.lookup currState dfaTrns
      nextState <- M.lookup c stateTrns
      first (c :) <$> runDfa' cs nextState
