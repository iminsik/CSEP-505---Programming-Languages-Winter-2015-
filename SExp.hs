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
          | Closure
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
-- 
-- Ok (NumS n1, Ok (NumS n2, ts2))  -> Ok (ListS [n1,n2], ts2]

-- CURRENT SEXP, REST TOKENS
parseSExp :: [Token] -> Result (SExp, [Token])
parseSExp tokens =
  case tokens of 
    [] -> Err "No More Tokens"
    t:ts -> 
      case t of
        Open b ->
          -- call parseListSHelper and return ListS
          case parseListSHelper ts of
            (sexp, tokens') -> Ok (sexp, tokens')
        NumTok n -> Ok (NumS n, [])
        IdTok s -> Ok (IdS s, [])

parseListSHelper :: [Token] -> (SExp, [Token])
parseListSHelper tokens = 
  case tokens of
    t:ts ->
      case t of
        -- If Open b is found , parse it with parseSExpHelper again
        Open b ->
          case parseListSHelper ts of
            (sexp, tokens) -> (ListS [sexp], tokens)
        -- If Close b is found, return merge Tokens and return ListS
        Close b -> (ListS [], []) 
        NumTok t' ->
          case parseListSHelper ts of 
            (ListS s', ts') -> (ListS ([NumS t'] ++ s'), ts')
        IdTok t' ->
          case parseListSHelper ts of 
            (ListS s', ts') -> (ListS ([IdS t'] ++ s'), ts')

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
