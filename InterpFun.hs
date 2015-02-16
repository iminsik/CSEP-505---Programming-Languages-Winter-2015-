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

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 -> return (PrimV ("partial:" ++ name)
                     (\arg2 ->
                       case (arg1, arg2) of
                        (NumV lv, NumV rv) -> return (op lv rv)
                        nonNum -> fail ("numeric op applied to: " ++
                                        (show nonNum)))))

-- (PrimV "+" (PrimV ("partial:+") (toResult (addVal))) 

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
              -- I don't use these wrapBinaryArithOp,
              -- though they are populated.
              -- I don't understand how I can apply them to arg1, arg2
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

-- interp (FunC var body) env = 
interp :: CExpr -> Env -> Result Val
interp (NumC n) env = Ok (NumV n)
interp (FunC var cexpr) env = Ok (FunV var cexpr env)
interp (VarC v) env = 
  case lookupBoundVar v env of
    Ok (a) -> Ok (a)
interp (AppC f a) env =
  case f of
    AppC f' a' -> 
      case interp f' env of
        Ok (PrimV name impl) -> 
          case interp a' env of
            Ok (NumV n) -> 
              case interp a env of
                Ok (NumV n') -> 
                  case name of 
                    "+" -> Ok (NumV ((+) n n'))
                    "*" -> Ok (NumV ((*) n n'))
                    "=" -> Ok (BoolV ((==) n n'))
                    "<" -> Ok (BoolV ((<) n n'))
                Err (msg) -> Err msg
            Err (msg) -> Err msg
        Err (msg) -> Err msg
    FunC var cexpr ->
      case interp f env of
        Ok (FunV var cexpr env) ->
          case interp a env of 
            Ok (NumV n) -> 
              case interp cexpr ((var, (NumV n)):env) of 
                Ok (NumV n') -> Ok (NumV n')
                Err (msg) -> Err msg
            Err (msg) -> Err msg
        Err (msg) -> Err msg

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
