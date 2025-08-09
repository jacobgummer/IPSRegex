module IPSRegex.Regex where

-- | Simple regular expressions.  Note that there is no concept of
-- matching a string, only matching a single character, which can then
-- be combined by concatenation.
data RegExp
  = -- | Nothing.
    RegEpsilon
  | -- | A literal character.
    RegChar Char
  | -- | Two expressions following each other.
    RegConcat RegExp RegExp
  | -- | Either of the given expressions.
    RegChoice RegExp RegExp
  | -- | Zero or more instances of the given expression.
    RegKleene RegExp
  | -- | One or more instances of the given expression.
    RegPlus RegExp
  | -- A range of characters.
    RegRange [Char]
  deriving (Show)
