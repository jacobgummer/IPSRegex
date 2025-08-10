module Main (main) where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import IPSRegex.DFA (DFA (..), nfaToDfa, runDfa)
import IPSRegex.NFA (NFA (..), regexpToNfa)
import IPSRegex.Parse (parseRegexp)
import IPSRegex.Regex (RegExp)

-- showSet :: (Show a) => S.Set a -> [Char]
-- showSet s = "{" ++ intercalate "," (map show (S.toList s)) ++ "}"

-- | Convert @s@ to a @RegExp@. Throws an error if @s@ isn't a valid regex.
strToRegex :: String -> RegExp
strToRegex s = case parseRegexp s of
  Left err -> error $ show err
  Right regex -> regex

-- | Get the longest prefix of the input string matching the given regex,
-- along with the remaining part of the string.
-- Returns @Nothing@ if no such prefix exists.
getLongestPrefix :: RegExp -> String -> Maybe (String, String)
getLongestPrefix regex str = runDfa str dfa
  where
    dfa = nfaToDfa . regexpToNfa $ regex

-- | Get the first match in an input string that matches the given regex.
-- Returns @Nothing@ if nothing in the string matches the regex.
getFirstMatch :: RegExp -> String -> Maybe String
getFirstMatch _ [] = Nothing
getFirstMatch regex str =
  case getLongestPrefix regex str of
    Nothing -> getFirstMatch regex (tail str)
    Just (match, _) -> pure match

-- | Get all parts of the input string that matches the given regex.
getAllMatches :: RegExp -> String -> [String]
getAllMatches regex str' = getAllMatches' str' []
  where
    getAllMatches' [] matches = reverse matches
    getAllMatches' str matches =
      case getLongestPrefix regex str of
        Nothing -> getAllMatches' (tail str) matches
        Just (match, rest) -> getAllMatches' rest (match : matches)

makeTransitions :: [(Int, Char, Int)] -> Map Int (Map Char Int)
makeTransitions = foldr f M.empty
  where
    f (s1, c, s2) acc =
      let trn = M.singleton s1 (M.singleton c s2)
       in M.unionWith M.union trn acc

-- | Fig. 1.10 in 'Introduction to Compiler Design'.
testDfa1 :: DFA
testDfa1 =
  DFA
    { dfaAlphabet = alph
    , dfaStartState = start
    , dfaStates = states
    , dfaAcceptingStates = accepting
    , dfaTransitions = trns
    }
  where
    alph = S.fromList "ab"
    states = S.fromList [0 .. 7]
    start = 0
    accepting = S.fromList [0, 6]
    trns =
      makeTransitions
        [ (0, 'a', 1)
        , (1, 'a', 4)
        , (1, 'b', 2)
        , (2, 'a', 3)
        , (2, 'b', 5)
        , (3, 'b', 1)
        , (4, 'a', 6)
        , (4, 'b', 5)
        , (5, 'a', 7)
        , (5, 'b', 2)
        , (6, 'a', 5)
        , (7, 'a', 0)
        , (7, 'b', 5)
        ]

-- | Figure from page 24 in 'Introduction to Compiler Design'.
testDfa2 :: DFA
testDfa2 =
  DFA
    { dfaAlphabet = alph
    , dfaStartState = start
    , dfaStates = states
    , dfaAcceptingStates = accepting
    , dfaTransitions = trns
    }
  where
    alph = S.fromList "ab"
    start = 1
    states = S.fromList [1, 2, 3]
    accepting = S.fromList [1, 2]
    trns =
      makeTransitions
        [ (1, 'a', 2)
        , (2, 'a', 1)
        , (2, 'b', 3)
        ]

-- TODO: Do things based on program input.
main :: IO ()
main = do
  putStrWInfo' "Regex" strReg
  putStrWInfo' "Regex (literal)" $ show regex
  putStrWInfo' "Number of states in NFA" $ show numStatesNFA
  putStrWInfo' "Number of states in DFA" $ show numStatesDFA
  putStrWInfo' "Input string" test
  putStrWInfo' "First match" $ showFstMatch regex test
  putStrWInfo "All matches" $ showAllMatches regex test
  where
    -- From AoC 2024, day 3.
    -- strReg = "don't\\(\\)|do\\(\\)|mul\\([0-9]+,[0-9]+\\)"
    -- test =
    --   "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)"
    --     <> "+mul(32,64](mul(11,8)undo()?mul(8,5))"
    strReg = "a(ab|ba)+"
    test = "aabbaab"
    regex = strToRegex strReg
    nfa = regexpToNfa regex
    dfa = nfaToDfa nfa
    numStatesDFA = S.size $ dfaStates dfa
    numStatesNFA = S.size $ nfaStates nfa

    putStrWInfo info s = putStrLn $ info <> ":\n" <> s
    putStrWInfo' info s = putStrLn $ info <> ":\n" <> s <> "\n"

    showFstMatch reg input = fromMaybe "no matches" (getFirstMatch reg input)

    showAllMatches reg input =
      let allMatches = getAllMatches reg input
       in if null allMatches
            then "no matches"
            else "\"" <> intercalate "\",\"" allMatches <> "\""
