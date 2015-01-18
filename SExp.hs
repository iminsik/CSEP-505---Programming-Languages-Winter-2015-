module SExp (SExp(NumS, IdS, ListS), parseSExp, tokenize, Result(Ok, Err)) where

import Token

-- Converts a string into a list of tokens.
tokenize :: String -> [Token]
--tokenize "" = []
  --[ fst ( fromJust (parseToken str) ) ]
    --           ++ (tokenize (snd (fromJust (parseToken str))))
tokenize str = case parseToken str of
  Nothing -> []
  (Just (tok, strRest)) -> tok : tokenize strRest

-- S-expression data definition.
data SExp = NumS Integer -- numeric expression
          | IdS String -- identifier
          | ListS [SExp] -- list of S-expressions
          deriving Eq

-- Show SExps in surface syntax.
instance Show SExp where
  show (NumS n) = show  n
  show (IdS name) = show name
  show (ListS sexps) = "(" ++ (unwords (map show sexps)) ++ ")"

-- Type for results of functions that can fail, such as parsing.
data Result a = Ok a -- Success
              | Err String -- Error with description
              deriving (Eq, Show)

-- Attempts to parse an S-expression from a list of tokens.
-- If successful, returns:
--    Ok (<parsed S-expression>, <REMAINING tokens>)
-- If not, returns:
--    Err <string describing problem encountered>
parseSExp :: [Token] 
  -> Result (
      SExp,   -- CURRENT SEXP
      [Token] -- REST TOKENS
  )

parseSExp tokens@(t:ts) =
  case tokens of 
    [] -> Err "No Tokens"
    _  -> 
      case t of
        NumTok n -> Ok (NumS n, ts)
        IdTok s -> Ok (IdS s, ts)
        Open Round ->
          case ts of
            Close Round:ss -> Ok (ListS [], ss)
            _ ->
              case parseSExp ts of
                Ok (sExpsym, tokens) -> 
                  case tokens of
                    Close Round:ss -> Ok (ListS [sExpsym], ss)
                    _ -> Err "Missing Close Round"
        Close Round -> Err "Missing Open Round"

-- What are passed test
--((foo))
--(foo)
--((1234))
--((-1234))
--()

-- Examples that should parse.
validExamples = [
  ("empty list", "()", ListS []), -- Done
  ("single id", "true", IdS "true"), -- Done
  ("positive num", "1234", NumS 1234), -- Done
  ("negative num", "-1234", NumS (-1234)), -- Done
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
