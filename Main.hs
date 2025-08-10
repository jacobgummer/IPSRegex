module Main (main) where

import Data.List (intercalate)
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
