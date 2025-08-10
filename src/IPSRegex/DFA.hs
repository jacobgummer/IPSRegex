module IPSRegex.DFA (nfaToDfa, runDfa, DFA (..)) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
import Data.Set (Set, (\\))
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

-- | Convert an NFA to a DFA using the subset construction algorithm described
-- in section 1.5.2 in 'Introduction to Compiler Design'.
--
-- Currently, the resulting DFA is not minimal.
nfaToDfa :: NFA -> DFA
nfaToDfa nfa = minimiseDfa dfa
  where
    nfaTrns = nfaTransitions nfa
    nfaAlph = nfaAlphabet nfa
    startStateS = Set.singleton $ nfaStartState nfa
    s0' = epsClosure nfaTrns startStateS
    (states, allTrns) = subsetConstruction s0' nfaTrns nfaAlph

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
        }

-- | Convert a map of transitions such that all keys in the outer map and
-- all values in the inner map are now integers instead of sets.
convertTrns ::
  (DFAState' -> DFAState) ->
  DFATransitions' ->
  DFATransitions
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
    moveAll s' = foldr (findStatesAndTransitions s') (Set.empty, M.empty) nfaAlph

    findStatesAndTransitions s' c acc@(states, trns) =
      let to = move s' c
       in if null to
            then acc
            else (Set.insert to states, trns `combine` connect s' c to)

    -- Calculate the (DFA) state that s' should reach when transitioning via c.
    move s' c = epsClosure nfaTrns . Set.unions $ Set.map (reachableVia c) s'

    -- Find every (NFA) state reachable from s via c.
    reachableVia c s =
      M.findWithDefault Set.empty (Symbol c)
        . M.findWithDefault M.empty s
        $ nfaTrns

-- TODO: Implement minimisation of DFAs.
-- See 'https://www.numberanalytics.com/blog/hopcroft-algorithm-guide'.
minimiseDfa :: DFA -> DFA
minimiseDfa dfa = id dfa
  where
    accepting = dfaAcceptingStates dfa
    nonAccepting = dfaStates dfa \\ accepting
    partition = [accepting, nonAccepting]

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
