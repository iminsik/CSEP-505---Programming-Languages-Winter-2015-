module InterpFun where

import SExp
import Expr
import Result

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> Result Val)  -- name and implementation

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (FunV var body env) = "(fun (" ++ var ++ ") " ++ (show body) ++ " | " ++
                             (show env) ++ ")"
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"

toResult :: Val -> Result Val
toResult (NumV n) = Ok (NumV n)

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 -> return (PrimV ("partial:" ++ name)
                     (\arg2 ->
                       case (arg1, arg2) of
                        (NumV lv, NumV rv) -> return (op lv rv)
                        nonNum -> fail ("numeric op applied to: " ++
                                        (show nonNum)))))

-- (PrimV "+" (PrimV ("partial:+") (op 1 2)))

addVal :: Integer -> Integer -> Val
addVal arg1 arg2 = NumV ((+) arg1 arg2)
multiVal :: Integer -> Integer -> Val
multiVal arg1 arg2 = NumV ((*) arg1 arg2)
equalVal :: Integer -> Integer -> Val
equalVal arg1 arg2 = BoolV ((==) arg1 arg2)
ltVal :: Integer -> Integer -> Val
ltVal arg1 arg2 = BoolV ((>) arg1 arg2)

-- Populate initialEnv ...
initialEnv :: Env
initialEnv = [("true", BoolV True), 
              ("false", BoolV False), 
              ("+", (wrapBinaryArithOp "+" addVal)),
              ("*", (wrapBinaryArithOp "*" multiVal)),
              ("=", (wrapBinaryArithOp "=" equalVal)),
              ("<", (wrapBinaryArithOp "<" ltVal))
              ]

lookupBoundVar :: Var -> Env -> Result Val
lookupBoundVar var env =
  case lookup var env of
      Just a -> Ok a
      Nothing -> Err "Variable is not bound."

---- interp expr env = Err "'interp' not yet implemented"
-- numbers and booleans eval to themselves
{-
interp (NumC n) initialEnv = Ok (NumV n)
interp (AppC (FunC var body) (NumC n)) initialEnv = 
  interp (FunC var body) ((var, NumV n):initialEnv)
interp (AppC (AppC op arg1) arg2) initialEnv = 
  case lookupBoundVar op initialEnv of
    Ok (PrimV opname op') -> case lookupBoundVar arg1 initialEnv of
      Ok arg1' -> case lookupBoundVar arg2 initialEnv of
        Ok arg2' -> Ok (NumV (op' arg1' arg2'))

interp (FunC var body) initialEnv = Err "Not implemented"
-}

-- interp (FunC var body) env = 
interp :: CExpr -> Env -> Result Val
interp (NumC n) env = Ok (NumV n)
interp (VarC v) env = 
  case lookupBoundVar v env of
    Ok (a) -> Ok (a)
interp (AppC f a) env =
  case interp f env of
    Ok (PrimV var op) -> Err "Primitive msg"
      --case interp a env of 
      --  Ok (NumV n) -> Ok (NumV (op (NumV n) (NumV 1)))
    Err (msg) -> Err msg
    --Ok val' -> case val' of
    --  PrimV name impl -> case interp a env of
    --    Ok (NumV val'') -> Ok (impl val'')
-- interpVal(desugarVal(parseExprVal(parseSExp (tokenize "(+ 1 2)")))) initialEnv
-- desugarVal(parseExprVal(parseSExp (tokenize "(+ 1 2)")))
-- Ok (AppC (AppC (VarC "+") (NumC 1)) (NumC 2))

-- we need to evaluate just the condition first
interp (IfC cond con alt) env =
  case interp cond env of
    -- and then chose the correct arm to interp based on the result
    Ok(BoolV True) -> interp con env
    Ok(BoolV False) -> interp alt env
    -- or find a type error
    Ok(_) -> Err (show cond ++ " is not a boolean")
    Err(msg) -> Err msg

interpVal :: Result CExpr -> Env -> Val
interpVal (Ok cexpr) env = case interp cexpr env of
  Ok val -> val

interpPrint :: Result Val -> Val
interpPrint (Ok val) = val