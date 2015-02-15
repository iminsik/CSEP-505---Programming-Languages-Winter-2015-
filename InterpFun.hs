module InterpFun where

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

{-
Ok (
  AppC 
  (
    FunC 
    "x" 
    (
      AppC 
      (
        FunC 
        "y" 
        (
          AppC 
          (
            AppC 
            (VarC "+") 
            (VarC "x")
          ) 
          (VarC "y")
        )
      ) 
      (NumC 2)
    )
  ) 
  (NumC 1)
)
-}

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 -> return (PrimV ("partial:" ++ name)
                     (\arg2 ->
                       case (arg1, arg2) of
                        (NumV lv, NumV rv) -> return (op lv rv)
                        nonNum -> fail ("numeric op applied to: " ++
                                        (show nonNum)))))

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
              ("<", (wrapBinaryArithOp "<" ltVal))]

lookupBoundVar :: Var -> Env -> Result Val
lookupBoundVar var env =
  case lookup var env of
      Just a -> Ok a
      Nothing -> Err "Variable is not bound."

interp :: CExpr -> Env -> Result Val
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
interp (NumC n) env = Ok (NumV n)
interp (VarC v) env = 
  case lookupBoundVar v env of
    Ok (a) -> Ok (a)
interp (AppC f a) env =
  case interp f env of
    Ok val' -> case val' of
      PrimV name impl -> case interp a env of
        Ok val'' -> Ok (impl val'')
-- we need to evaluate the arguments for a binop and make sure they're numbers
{-
interp (BinOpE op arg1 arg2) =
  case interp arg1 of
    Ok(NumV n1) -> case interp arg2 of
      Ok(NumV n2) -> case op of
        -- now we need to find out what operation we're doing
        Add -> Ok (NumV (n1 + n2))
        Mult -> Ok (NumV (n1 * n2))
        Equal -> Ok (BoolV (n1 == n2))
        Lt -> Ok (BoolV (n1 < n2))
      Ok(_) -> Err (show arg2 ++ " is not a number")
      Err(msg) -> Err msg
    Ok(_) -> Err (show arg1 ++ " is not a number")
    Err(msg) -> Err msg
-}
-- we need to evaluate just the condition first
interp (IfC cond con alt) env =
  case interp cond env of
    -- and then chose the correct arm to interp based on the result
    Ok(BoolV True) -> interp con env
    Ok(BoolV False) -> interp alt env
    -- or find a type error
    Ok(_) -> Err (show cond ++ " is not a boolean")
    Err(msg) -> Err msg

