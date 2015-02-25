module Interp where

import SExp

-- Operations to support.
-- Concrete syntax:
-- <op> ::= + | * | = | <
data BinOp = Add | Mult | Equal | Lt deriving (Eq, Show)

-- Expressions.
-- Concrete syntax:
-- <e> ::= <number>
--       | true
--       | false
--       | (<op> <e> <e>)
--       | (if <e> <e> <e>)
data Expr = NumE Integer
          | BoolE Bool
          | BinOpE BinOp Expr Expr
          | IfE Expr Expr Expr
          deriving (Eq, Show)

parseExpr :: SExp -> Result Expr
-- numbers are simple
parseExpr (NumS n) = Ok (NumE n)
-- booleans are just special identifiers
parseExpr (IdS "true") = Ok (BoolE True)
parseExpr (IdS "false") = Ok (BoolE False)
-- all other identifiers are errors
parseExpr (IdS _) = Err "unrecognized identifier"
-- binops are 3 element lists
parseExpr (ListS [op,arg1,arg2]) =
  case parseBinOp op of
    Ok(op') -> case parseExpr arg1 of
      Ok(arg1') -> case parseExpr arg2 of
        Ok(arg2') -> Ok (BinOpE op' arg1' arg2')
        Err(msg) -> Err msg 
      Err(msg) -> Err msg 
    Err(msg) -> Err msg 
-- ifs are 4 element lists
parseExpr (ListS [IdS "if",cond,con,alt]) =
  case parseExpr cond of
    Ok(cond') -> case parseExpr con of
      Ok(con') -> case parseExpr alt of
        Ok(alt') -> Ok (IfE cond' con' alt')
        Err(msg) -> Err msg
      Err(msg) -> Err msg
    Err(msg) -> Err msg
-- anything else is an error
parseExpr _ = Err "unrecognized expression"

parseBinOp (IdS "+") = Ok Add
parseBinOp (IdS "*") = Ok Mult
parseBinOp (IdS "=") = Ok Equal
parseBinOp (IdS "<") = Ok Lt
parseBinOp _ = Err "unrecognized operation"


validParseExamples = [
  ("non-trivial example", "(if (= (* 2 3) (+ 5 1)) 7 10)",
   IfE (BinOpE Equal (BinOpE Mult (NumE 2) (NumE 3))
        (BinOpE Add (NumE 5) (NumE 1))) (NumE 7) (NumE 10))
  -- Feel free to add your own examples ...
  ]

checkValidParseExample (description, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> parseExpr sexp == Ok expected
    _ -> False)

interp :: Expr -> Result Expr
-- numbers and booleans eval to themselves
interp (NumE n) = Ok (NumE n)
interp (BoolE b) = Ok (BoolE b)
-- we need to evaluate the arguments for a binop and make sure they're numbers
interp (BinOpE op arg1 arg2) =
  case interp arg1 of
    Ok(NumE n1) -> case interp arg2 of
      Ok(NumE n2) -> case op of
        -- now we need to find out what operation we're doing
        Add -> Ok (NumE (n1 + n2))
        Mult -> Ok (NumE (n1 * n2))
        Equal -> Ok (BoolE (n1 == n2))
        Lt -> Ok (BoolE (n1 < n2))
      Ok(_) -> Err (show arg2 ++ " is not a number")
      Err(msg) -> Err msg
    Ok(_) -> Err (show arg1 ++ " is not a number")
    Err(msg) -> Err msg
-- we need to evaluate just the condition first
interp (IfE cond con alt) =
  case interp cond of
    -- and then chose the correct arm to interp based on the result
    Ok(BoolE True) -> interp con
    Ok(BoolE False) -> interp alt
    -- or find a type error
    Ok(_) -> Err (show cond ++ " is not a boolean")
    Err(msg) -> Err msg

interpVal :: Result CExpr -> Env -> Val
interpVal (Ok cexpr) env = case interp cexpr env of
  Ok val -> val