module IPSRegex.DFA (nfaToDfa, runDfa, DFA (..)) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as S
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
epsClosure trns m = epsClosure' (S.elems m) m
  where
    epsClosure' [] clsr = clsr
    epsClosure' (x : unmarked) clsr =
      let fMx = f_M x
          newClsr = clsr <> fMx
          newUnmarked = S.elems $ fMx \\ clsr
       in epsClosure' (newUnmarked ++ unmarked) newClsr

    f_M x = S.insert x $ epsTransitions trns (S.singleton x)

-- | Determine which states can be reached from @states@ via a single
-- ε-transition.
epsTransitions :: NFATransitions -> Set NFAState -> Set NFAState
epsTransitions trns states = S.unions $ S.map reachableViaEps states
  where
    reachableViaEps s =
      M.findWithDefault S.empty Epsilon . M.findWithDefault M.empty s $ trns

-- | Convert an NFA to a DFA using the subset construction algorithm described
-- in section 1.5.2 in 'Introduction to Compiler Design'.
--
-- Currently, the resulting DFA is not minimal.
nfaToDfa :: NFA -> DFA
nfaToDfa nfa = minimiseDfa dfa
  where
    nfaTrns = nfaTransitions nfa
    nfaAlph = nfaAlphabet nfa
    startStateS = S.singleton $ nfaStartState nfa
    s0' = epsClosure nfaTrns startStateS
    (states, allTrns) = subsetConstruction s0' nfaTrns nfaAlph

    allStates = S.insert s0' states

    nfaAcc = nfaAcceptingState nfa

    accepting = S.filter (S.member nfaAcc) allStates

    idStatePairs = M.fromList $ zip (S.elems allStates) [0 ..]

    converter = (M.!) idStatePairs
    idsAllTrns = convertTrns converter allTrns
    idsAllStates = S.fromList $ M.elems idStatePairs
    idS0' = converter s0'
    idsAccepting = S.map converter accepting

    dfa =
      DFA
        { dfaStates = idsAllStates
        , dfaStartState = idS0'
        , dfaAcceptingStates = idsAccepting
        , dfaTransitions = idsAllTrns
        }

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
  subsetConstruction' [s0'] S.empty M.empty
  where
    subsetConstruction' [] states trns = (states, trns)
    subsetConstruction' (s' : unmarked) states trns =
      let (states', trns') = moveAll s'
          newStates = states <> states'
          newUnmarked = S.elems $ states' \\ states
          newTrns = trns `combine` trns'
       in subsetConstruction' (newUnmarked ++ unmarked) newStates newTrns

    -- Make a transition from one DFA state to another via c.
    connect from c to = M.singleton from (M.singleton c to)

    -- Combine/union two 'DFATransitions'-mappings.
    combine = M.unionWith M.union

    -- For each character c in the original NFA's alphabet, find which state
    -- s' should transition to via c.
    moveAll s' = foldr (findStatesAndTransitions s') (S.empty, M.empty) nfaAlph

    findStatesAndTransitions s' c acc@(states, trns) =
      let to = move s' c
       in if null to
            then acc
            else (S.insert to states, trns `combine` connect s' c to)

    -- Calculate the (DFA) state that s' should reach when transitioning via c.
    move s' c = epsClosure nfaTrns . S.unions $ S.map (reachableVia c) s'

    -- Find every (NFA) state reachable from s via c.
    reachableVia c s =
      M.findWithDefault S.empty (Symbol c)
        . M.findWithDefault M.empty s
        $ nfaTrns

-- TODO: Implement minimisation of DFAs.
-- See 'https://www.numberanalytics.com/blog/hopcroft-algorithm-guide'.
-- Remember to make the move function total first.
minimiseDfa :: DFA -> DFA
minimiseDfa = id

-- This was also inspired by Troels Henriksens 'runDFA' function
-- in 'https://sigkill.dk/hacks/scripts/HsGrep.hs'.
runDfa :: String -> DFA -> Maybe (String, String)
runDfa str' dfa = runDfa' str' (dfaStartState dfa)
  where
    dfaTrns = dfaTransitions dfa
    accepting = dfaAcceptingStates dfa

    isAccepting st = st `S.member` accepting

    runDfa' str currState
      | isAccepting currState =
          -- If the current state is accepting, we still try to consume the
          -- next character(s), but if this fails, we stop here.
          consume str currState <|> Just ("", str)
      | otherwise = consume str currState

    consume [] _ = Nothing
    consume str currState = go str =<< M.lookup currState dfaTrns

    -- If there is a transition from the current state via c, prepend c
    -- to all of the next matched characters in the input. Otherwise, go
    -- will return Nothing.
    go (c : cs) currStTrns =
      first (c :) <$> (runDfa' cs =<< M.lookup c currStTrns)
