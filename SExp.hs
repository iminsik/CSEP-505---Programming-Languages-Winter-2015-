module SExp (SExp(NumS, IdS, ListS), parseSExp, tokenize, Result(Ok, Err)) where

import Token

-- Converts a string into a list of tokens.
tokenize :: String -> [Token]
tokenize str = case parseToken str of
  Nothing -> []
  Just (tok, str') -> tok : tokenize str'

-- S-expression data definition.
data SExp = NumS Integer -- numeric expression
          | IdS String -- identifier
          | ListS [SExp] -- list of S-expressions
          deriving Eq

-- Show SExps in surface syntax.
instance Show SExp where
  show (NumS n) = show n
  show (IdS name) = name
  show (ListS sexps) = "(" ++ (unwords (map show sexps)) ++ ")"

-- Type for results of functions that can fail, such as parsing.
data Result a = Ok a -- Success
              | Err String -- Error with description
              deriving (Eq, Show)

-- Attempts to parse an S-expression from a list of tokens.
-- If successful, returns:
--    Ok (<parsed S-expression>, <remaining tokens>)
-- If not, returns:
--    Err <string describing problem encountered>
parseSExp :: [Token] -> Result (SExp, [Token])
-- no tokens = no sexp
parseSExp [] = Err "empty"
-- numbers and ids are simple
parseSExp ((NumTok n):ts) = Ok (NumS n, ts)
parseSExp ((IdTok s):ts) = Ok (IdS s, ts)
-- when we find an open brace
-- try to parse until we find the corresponding close
parseSExp ((Open b):ts) = toClosed b [] ts
-- if we find a closing brace before an opening one, error
parseSExp ((Close b):ts) = Err ("unopened " ++ show b)

toClosed :: Brace -> [SExp] -> [Token] -> Result (SExp, [Token])
-- ran out of tokens = unclosed brace
toClosed b sexps [] = Err ("no closing " ++ show b)
-- found the closing brace
toClosed b sexps ((Close b'):ts)
  | b == b' = Ok (ListS (reverse sexps), ts)
-- mismatched braces
  | b /= b' = Err ("mismatched braces" ++ show b ++ " " ++ show b')
-- non-brace parsable token goes on the stack
toClosed b sexps ts = case parseSExp ts of
  Ok(sexp, ts) -> toClosed b (sexp:sexps) ts
-- unparsable token = error
  Err(msg) -> Err msg

-- Examples that should parse.
validExamples = [
  ("empty list", "()", ListS []),
  ("single id", "true", IdS "true"),
  ("positive num", "1234", NumS 1234),
  ("negative num", "-1234", NumS (-1234)),
  ("mixed list", "(foo () 4 (7 false))",
   ListS [IdS "foo", ListS [], NumS 4, ListS [NumS 7, IdS "false"]])
  ]

-- Examples that should not parse.
invalidExamples = [
  ("empty", ""),
  ("close without open", ")"),
  ("no close", "(3 4"),
  ("mismatched brace types", "(foo bar]")
  ]

-- Check a single valid example.
checkValid (description, input, expected) =
  (description, (parseSExp (tokenize input)) == Ok (expected, []))

-- Check a single invalid example.
checkInvalid (description, input) =
  (description, case parseSExp (tokenize input) of
                 -- Just check that it failed; don't try to match the
                 -- specific error message.
                 Err _ -> True
                 _ -> False)
