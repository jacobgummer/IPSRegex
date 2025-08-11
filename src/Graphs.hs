module Graphs where

import Control.Monad (forM_)
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (chr, ord)
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import IPSRegex.DFA
import IPSRegex.NFA
import IPSRegex.Parse
import IPSRegex.Regex
import Text.Parsec (ParseError, SourceName)

class (Ord a) => Next a where
  next :: a -> a

instance Next TransSymbol where
  next Epsilon = Epsilon
  next (Symbol c) = Symbol $ chr $ ord c + 1

instance Next Char where
  next c = chr (ord c + 1)

ranges :: (Next a, Ord b) => [(a, b)] -> [([(a, a)], b)]
ranges = map ranges' . M.toList . foldl arrange M.empty
  where
    arrange m (k, v) = M.insertWith (++) v [k] m
    ranges' (v, ks) = (foldl ranges'' [] $ sort ks, v)
    ranges'' [] x = [(x, x)]
    ranges'' ((y1, y2) : ys) x
      | next y2 == x = (y1, x) : ys
      | otherwise = (x, x) : (y1, y2) : ys

-- | Escape the double-quotes in a string so it can be put in a
-- Graphviz string.
escape :: String -> String
escape [] = []
escape ('"' : s) = "\\\"" ++ escape s
escape (c : s) = c : escape s

translabel :: (Next a) => (a -> String) -> [(a, a)] -> String
translabel f [(x, y)]
  | x == y = f x
  | otherwise = "[" ++ f x ++ "-" ++ f y ++ "]"
translabel f xs = "[" ++ concatMap charclass xs ++ "]"
  where
    charclass (x, y)
      | x == y = f x
      | otherwise = f x ++ "-" ++ f y

statelabel :: String -> Int -> Bool -> String
statelabel k i a =
  k
    ++ "[label=\""
    ++ show i
    ++ "\""
    ++ (if a then ", shape=doublecircle" else "shape=circle")
    ++ "]\n"

printNFA :: NFA -> String
printNFA nfa = evalState (execWriterT printNFA') (M.empty, 0 :: Int)
  where
    node k = do
      (s, v) <- get
      case M.lookup k s of
        Just v' -> return v'
        Nothing -> do
          let v' = v + 1
              v'' = 'S' : show v'
          tell $ statelabel v'' v' $ k == nfaAcceptingState nfa
          put (M.insert k v'' s, v')
          return v''
    symbol Epsilon = "epsilon"
    symbol (Symbol c) = [c]
    trans (from, ts) =
      forM_ (ranges $ M.toList ts) $ \(clss, tos) ->
        forM_ (S.toList tos) $ \to -> do
          from' <- node from
          to' <- node to
          tell $
            from'
              ++ "->"
              ++ to'
              ++ " [label=\""
              ++ escape (translabel symbol clss)
              ++ "\"]\n"
    printNFA' = do
      tell "digraph nfa {\nrankdir=TD\nEmp [style=invisible]\n"
      mapM_ trans $ M.toList $ nfaTransitions nfa
      tell "}\n"

printDFA :: Bool -> DFA -> String
printDFA minimise dfa = evalState (execWriterT printDFA') (M.empty, 0 :: Int)
  where
    dfa' = if minimise then minimiseDfa dfa else dfa
    node k = do
      (s, v) <- get
      case M.lookup k s of
        Just v' -> return v'
        Nothing -> do
          let v' = v + 1
              v'' = 'S' : show v'
          tell $ statelabel v'' v $ k `S.member` dfaAcceptingStates dfa'
          put (M.insert k v'' s, v')
          return v''
    trans (from, ts) =
      forM_ (ranges $ M.toList ts) $ \(clss, to) -> do
        from' <- node from
        to' <- node to
        tell $
          from'
            ++ "->"
            ++ to'
            ++ " [label=\""
            ++ escape (translabel (: []) clss)
            ++ "\"]\n"
    printDFA' = do
      tell "digraph dfa {\nrankdir=LR\nEmp [style=invisible]\n"
      mapM_ trans $ M.toList $ dfaTransitions dfa'
      tell "}\n"

compileRegexp :: String -> Either ParseError NFA
compileRegexp r = regexpToNfa <$> parseRegexp r

printRegexpDFA :: String -> Bool -> String
printRegexpDFA regex minimise = printDFA minimise $ nfaToDfa nfa
  where
    nfa = case compileRegexp regex of
      Left e -> error $ show e
      Right v -> v

printRegexpNFA :: String -> String
printRegexpNFA regex = printNFA nfa
  where
    nfa = case compileRegexp regex of
      Left e -> error $ show e
      Right v -> v
