module Expr (Var, Expr(..), CExpr(..), parseExpr, desugar, checkIds) where

import Data.List
import Result
import SExp

type Var = String

-- Expression syntax:
-- <e> ::= <number>
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x ...) <e>)
--       | (<e> <e> ...)
--       | (with* ([x <e>] ...) <e>)
data Expr = NumE Integer
          | IfE Expr Expr Expr
          | VarE Var
          | FunE [Var] Expr
          | AppE [Expr]
          | WithStarE [(Var, Expr)] Expr
          deriving (Eq, Show)

-- Core language (desugared from Expr).
-- <e> ::= <number>
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x) <e>)
--       | (<e> <e>)
data CExpr = NumC Integer
           | IfC CExpr CExpr CExpr
           | VarC Var
           | FunC Var CExpr
           | AppC CExpr CExpr
           deriving (Eq, Show)

parseExpr :: SExp -> Result Expr
parseExpr (NumS n)  = Ok(NumE n)
parseExpr (IdS var) = Ok(VarE var)

-- ifs are 4 element lists
parseExpr (ListS [IdS "if",cond,con,alt]) =
  case parseExpr cond of
    Ok(cond') -> case parseExpr con of
      Ok(con') -> case parseExpr alt of
        Ok(alt') -> Ok (IfE cond' con' alt')
        Err(msg) -> Err msg
      Err(msg) -> Err msg
    Err(msg) -> Err msg


parseExpr (ListS [IdS "fun",ListS arg,expr]) =
  case parseVarList arg of
    Ok(arg')  -> case parseExpr expr of
      Ok(expr') -> Ok(FunE arg' expr')
      Err(msg)  -> Err msg
    Err(msg)  -> Err msg

parseExpr (ListS [IdS "with*",ListS arg,expr]) =
  case parseBindVarList arg of
    Ok(arg')  -> case parseExpr expr of
      Ok(expr') -> Ok(WithStarE arg' expr')
      Err(msg)  -> Err msg
    Err(msg)  -> Err msg

parseExpr (ListS s) =
  case parseExprList s of
    Ok (s') -> Ok (AppE s')
    Err (msg) -> Err msg

parseExprList :: [SExp] -> Result [Expr]
parseExprList li = 
  case li of 
    x:xs  -> case parseExpr x of
      Ok x' -> case parseExprList xs of
        Ok xs' -> Ok (x':xs')
        Err (msg) -> Err msg
      Err (msg) -> Err msg
    _ -> Ok []

parseVar :: SExp -> Result Var
parseVarList :: [SExp] -> Result [Var]

parseVar (IdS s) = Ok s
parseVar _ = Err "Not an Identifier"

parseVarList s = 
  case s of
    x:xs  -> case parseVar x of
      Ok (x') -> case parseVarList xs of
        Ok (xs')  -> Ok (x':xs')
        Err (msg) -> Err msg
      Err (msg) -> Err msg
    _ -> Ok []
-- parseExprHelper :: SExp -> 
-- parseExpr sexp = Err "parseExpr not implemented yet"

parseBindVarList :: [SExp] -> Result [(Var, Expr)]
parseBindVar :: SExp -> Result(Var, Expr)

parseBindVarList li =
  case li of
    x:xs -> case parseBindVar x of
      Ok (x',expr') -> case parseBindVarList xs of
        Ok ret  -> Ok ((x',expr'):ret)
        Err (msg) -> Err msg
      Err (msg) -> Err msg
    _ -> Ok []

parseBindVar sexp = 
  case sexp of
    ListS (x:xs:[])  -> case parseVar x of
      Ok (x') -> case parseExpr xs of
        Ok (xs')  -> Ok (x', xs')
        Err (msg) -> Err msg
      Err (msg) -> Err msg
    _ -> Err "Invalid Bind Var"

desugar :: Expr -> Result CExpr
-- desugar expr = Err "desugar not implemented yet"
desugar (NumE n) = Ok (NumC n)
desugar (VarE n) = Ok (VarC n)

{-
desugar (IfE cond con alt) = 
  case desugar cond of
    Ok(cond') -> case desugar con of
      Ok(con') -> case desugar alt of
        Ok(alt') -> Ok (IfC cond' con' alt')
        Err(msg) -> Err msg
      Err(msg) -> Err msg
    Err(msg) -> Err msg

desugar (AppE expr) =
  case expr of
    x:[] -> case desugar x of 
      Ok (x') -> Ok (x')
    x:xslist -> case desugar x of 
      Ok (x') -> case desugar (AppE xslist) of 
          Ok (xslist') -> Ok (AppC (x') xslist')
    _ -> Err "No CExpr"
-}

desugar (AppE expr) =
  case expr of
    x:[] -> case desugar x of 
      Ok (x') -> Ok (x')
    x:xs:[] -> case desugar x of 
      Ok (x') -> case desugar xs of
        Ok (xs') -> Ok (AppC (x') (xs'))
    x:xs:xslist -> case desugar x of 
      Ok (x') -> case desugar xs of
        Ok (xs') -> case desugar (AppE xslist) of 
          Ok (xslist') -> Ok (AppC (AppC (x') (xs')) xslist')
    _ -> Err "No CExpr"

{-
parseExpr (FunE arg' expr') =
  case arg' of
    x:xs  -> 
      Ok(expr') -> Ok(FunE arg' expr')
      Err(msg)  -> Err msg
    Err(msg)  -> Err msg
-}
-- desugar (WithStarE bindVars expr) = Ok (FunC "x" (NumC 1))

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr = Err "checkIds not implemented yet"

parseExprVal :: Result (SExp, [a]) -> Result Expr
parseExprVal (Ok sexp@(t,ts)) = parseExpr (t)
desugarVal :: Result Expr -> Result CExpr
desugarVal (Ok expr) = desugar (expr)