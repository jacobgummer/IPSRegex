module IPSRegex.Parse (parseRegexp) where

import Control.Applicative (Alternative ((<|>)))
import IPSRegex.Regex (RegExp (..))
import Text.Parsec (
  ParseError,
  SourceName,
  anyChar,
  between,
  chainl,
  chainl1,
  chainr1,
  char,
  eof,
  noneOf,
  parse,
  try,
 )
import Text.Parsec.String (Parser)

-- This parser is basically a copy of the one made by Troels
-- Henriksen in 'https://sigkill.dk/hacks/scripts/HsGrep.hs'.
regexp :: Parser RegExp
regexp = chainr1 simple (char '|' >> pure RegChoice)
  where
    simple = chainl term (pure RegConcat) RegEpsilon
    term =
      kleene (between (char '(') (char ')') regexp)
        <|> kleene (between (char '[') (char ']') clss)
        <|> text
    clss = chainl1 (comp <|> chr) (pure RegChoice)
    comp = do
      (c1, c2) <- try $ ((,) <$> clchr) <*> (char '-' *> clchr)
      if c1 > c2
        then fail $ "Empty range " <> [c1] <> "-" <> [c2]
        else return $ RegRange [c1 .. c2]
    text = chainl1 (kleene chr) (pure RegConcat)
    clchr = noneOf "[]" <|> char '\\' *> anyChar
    chr = RegChar <$> (char '\\' *> anyChar <|> noneOf "()|*[]+")
    kleene p = do
      s <- p
      (char '*' >> return (RegKleene s))
        <|> (char '+' >> return (RegPlus s))
        <|> return s

parseRegexp :: String -> Either ParseError RegExp
parseRegexp = parse (regexp <* eof) "command-line"
