module IPSRegex.NFA where

import Control.Monad.State (MonadState (get, put), evalState)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import IPSRegex.Regex (RegExp (..))

data TransSymbol
  = Symbol Char
  | Epsilon
  deriving (Ord, Eq)

instance Show TransSymbol where
  show (Symbol c) = show c
  show Epsilon = "ε"

convertSymToChar :: TransSymbol -> Char
convertSymToChar (Symbol c) = c
convertSymToChar _ = error "Cannot extract anything from ε"

type NFAState = Int

-- | Transitions from a single NFA state.
type NFAStateTransitions = Map TransSymbol (Set NFAState)

-- | The outgoing transitions for the states in the NFA.
type NFATransitions = Map NFAState NFAStateTransitions

type Alphabet = Set Char

data NFA = NFA
  { nfaStates :: Set NFAState
  , nfaStartState :: NFAState
  , nfaTransitions :: NFATransitions
  , nfaAcceptingState :: NFAState
  , nfaAlphabet :: Alphabet
  }

-- TODO: Implement optimisation for ε-regexes in regexpToNfa'.

-- | Convert a regular expression to an NFA. This algorithm directly
-- follows fig. 1.4 (page 10) in 'Introduction to Compiler Design',
-- along with the optimisations outlined in fig. 1.6 (page 11)
-- (excluding the optimisation for ε-regexes).
regexpToNfa :: RegExp -> NFA
regexpToNfa regex =
  flip evalState 1 $ do
    start <- newNFAState
    (states', trns', penultimate, lastSym) <- regexpToNfa' regex start
    final <- newNFAState
    let lastTransition = connect penultimate lastSym final
        trns = trns' `combine` lastTransition
        alphabet = getAlphabet trns
        states = states' <> S.fromList [start, final]
    pure
      NFA
        { nfaStates = states
        , nfaStartState = start
        , nfaTransitions = trns
        , nfaAcceptingState = final
        , nfaAlphabet = alphabet
        }
  where
    -- OPTIM: This could simply be part of the output of regexpToNfa'.
    getAlphabet = S.fromList . map convertSymToChar . getSymbols

    getSymbols = concatMap (M.keys . M.filterWithKey notEps) . M.elems

    notEps k _ = k /= Epsilon

    newNFAState = do
      i <- get
      put (i + 1)
      pure i

    -- Make a transition from one NFA state to another via sym.
    connect from sym to = M.singleton from (M.singleton sym (S.singleton to))

    -- Combine/union two 'NFATransitions'-mappings.
    combine = M.unionWith (M.unionWith (<>))

    -- OPTIM: Doesn't use ε-optimisation (page 11).
    regexpToNfa' RegEpsilon fstSt =
      pure (S.empty, M.empty, fstSt, Epsilon)
    regexpToNfa' (RegChar c) fstSt =
      pure (S.empty, M.empty, fstSt, Symbol c)
    regexpToNfa' (RegConcat r1 r2) fstSt = do
      (states1, trns1, finalSt1, finalSym1) <- regexpToNfa' r1 fstSt
      s <- newNFAState
      (states2, trns2, finalSt2, finalSym2) <- regexpToNfa' r2 s
      let cnn = connect finalSt1 finalSym1 s
      pure
        ( S.insert s (states1 <> states2)
        , trns1 `combine` trns2 `combine` cnn
        , finalSt2
        , finalSym2
        )
    regexpToNfa' (RegChoice r1 r2) fstSt = do
      s1 <- newNFAState
      s2 <- newNFAState
      (states1, trns1, finalSt1, finalSym1) <- regexpToNfa' r1 s1
      (states2, trns2, finalSt2, finalSym2) <- regexpToNfa' r2 s2
      s3 <- newNFAState
      let cnn1 = connect fstSt Epsilon s1
          cnn2 = connect fstSt Epsilon s2
          cnn3 = connect finalSt1 finalSym1 s3
          cnn4 = connect finalSt2 finalSym2 s3
      pure
        ( states1 <> states2 <> S.fromList [s1, s2, s3]
        , trns1
            `combine` trns2
            `combine` cnn1
            `combine` cnn2
            `combine` cnn3
            `combine` cnn4
        , s3
        , Epsilon
        )
    regexpToNfa' (RegKleene r) fstSt = do
      s <- newNFAState
      (states, trns, finalSt, finalSym) <- regexpToNfa' r s
      let cnn1 = connect fstSt Epsilon s
          cnn2 = connect finalSt finalSym fstSt
      pure
        ( S.insert s states
        , trns `combine` cnn1 `combine` cnn2
        , fstSt
        , Epsilon
        )
    regexpToNfa' (RegPlus r) fstSt = do
      s1 <- newNFAState
      s2 <- newNFAState
      (states, trns, finalSt, finalSym) <- regexpToNfa' r s1
      let cnn1 = connect fstSt Epsilon s1
          cnn2 = connect finalSt finalSym s2
          cnn3 = connect s2 Epsilon fstSt
      pure
        ( states <> S.fromList [s1, s2]
        , trns `combine` cnn1 `combine` cnn2 `combine` cnn3
        , s2
        , Epsilon
        )
    regexpToNfa' (RegRange range) fstSt = do
      s <- newNFAState
      let symRange = map Symbol range
          connect' c = connect fstSt c s
          trns = foldr (combine . connect') M.empty symRange
      pure (S.singleton s, trns, s, Epsilon)
