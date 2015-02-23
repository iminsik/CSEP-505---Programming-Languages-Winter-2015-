module InterpCont where

import Expr
import Result
import SExp
import Token

-- Values resulting from interpreting an expression.
data Val = NumV Integer -- a numeric constant
         | BoolV Bool -- a boolean constant
         | StringV String -- a string constant
         | EmptyV -- an empty list
         | ConsV Val Val -- a non-empty list
         | PairV Val Val -- a pair
         | FunV Var CExpr Env -- a function closure
         | PrimV String (Val -> Cont -> Result Val)  -- primitive: name and implementation

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (StringV s) = show s
  show EmptyV = "empty"
  show (ConsV h t) = "(cons " ++ (show h) ++ " " ++ (show t) ++ ")"
  show (PairV f s) = "(" ++ (show f) ++ ", " ++ (show s) ++ ")"
  show (FunV var body _) = "(fun (" ++ var ++ ") " ++ (show body) ++ ")"
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"

data Cont = DoneK
          | IfK CExpr CExpr Env Cont
          | AppFunK CExpr Env Cont
          | AppArgK Val Cont
          deriving Show

handleError :: Cont -> Val -> Result Val
handleError _ = \val -> Err ("unhandled error: " ++ (show val))

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 k -> callK k (PrimV ("partial:" ++ name)
                        (\arg2 k ->
                          case (arg1, arg2) of
                           (NumV lv, NumV rv) -> callK k (op lv rv)
                           nonNum -> handleError k (StringV (name ++ " applied to: " ++
                                                             (show nonNum))))))

add = wrapBinaryArithOp "+" (\x y -> (NumV (x+y)))
mult = wrapBinaryArithOp "*" (\x y -> (NumV (x*y)))
equal = wrapBinaryArithOp "=" (\x y -> (BoolV (x == y)))
less = wrapBinaryArithOp "<" (\x y -> (BoolV (x < y)))

unimplemented name = PrimV name (\v k -> Err (name ++ ": unimplemented"))

cons = unimplemented "cons"
consP = unimplemented "cons?"
emptyP = unimplemented "empty?"
first = unimplemented "first" 
rest = unimplemented "rest" 

pair = unimplemented "pair" 
pairFst = unimplemented "fst" 
pairSnd = unimplemented "snd" 

raise = unimplemented "raise" 
callWithHandler = unimplemented "call-with-handler"
callWithContext = unimplemented "call-with-context"
getContext = unimplemented "get-context"
callCc = unimplemented "call/cc"

bind prim@(PrimV name fn) = (name, prim)
bind nonPrim = error ("cannot bind " ++ (show nonPrim))

-- Populate initialEnv ...
initialEnv :: Env
initialEnv = [
  ("true", BoolV True),
  ("false", BoolV False),
  ("empty", EmptyV)] ++
  (map bind [add, mult, equal, less, emptyP, first, rest, cons,
             consP, pair, pairFst, pairSnd, callCc, callWithContext,
             getContext, callWithHandler, raise])

interp :: CExpr -> Env -> Cont -> Result Val
interp expr env k =
  case expr of
   NumC n -> callK k (NumV n)
   StringC s -> callK k (StringV s)
   FunC var body -> callK k (FunV var body env)
   VarC v ->
     case lookup v env of
      Nothing -> Err ("unbound id: " ++ v)
      Just val -> callK k val
   IfC cond cons alt -> interp cond env (IfK cons alt env k)
   AppC fun arg -> interp fun env (AppFunK arg env k)

callK :: Cont -> Val -> Result Val
callK DoneK val = Ok val
callK (IfK cons alt env k) val =
     case val of
      BoolV True -> interp cons env k
      BoolV False -> interp alt env k
      nonBool -> Err ("`if` expected bool, got: " ++ (show nonBool))

callK (AppFunK arg env k) fv =
  interp arg env (AppArgK fv k)
callK (AppArgK fv k) av = apply fv av k
  {-
  case fv of
    FunV var body closEnv -> 
      interp body ((var, av):closEnv) k
    PrimV fn op -> op av k
    nonFun -> Err ("Non-Function: " ++ (show nonFun))
  -}

parseCheckAndInterpStr :: String -> Result Val
parseCheckAndInterpStr str =
  let toks = tokenize str
      initialIds = map fst initialEnv in
  do (sexp, _) <- parseSExp toks
     expr <- parseExpr sexp
     cexp <- desugar expr
     checkIds initialIds (["fun", "if", "with*"] ++ initialIds) cexp
     interp cexp initialEnv DoneK

apply :: Val -> Val -> Cont -> Result Val
apply fv val k =
  case fv of
    FunV var body closEnv -> 
      interp body ((var, val):closEnv) k
    PrimV fn op -> op val k
    nonFun -> Err ("Non-Function: " ++ (show nonFun))

