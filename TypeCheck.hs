module TypeCheck (checkType, parseAndCheckStr) where

import Data.Char
import Data.List
import Expr
import Result
import SExp -- used only by parseAndCheckStr
import Token -- used only by parseAndCheckStr

-- Problem 2.
freeTypeVars :: Type -> [TVar] -> [TVar]
freeTypeVars ty bound = -- [] -- implement me!
  case ty of 
    BoolT -> []
    NumT -> []
    StringT -> []
    VarT a -> 
      case bound of 
        [] -> [a]
        _ ->
          if (elem a bound) then []
          else [a]
    ListT a1' -> 
      (freeTypeVars a1' bound)
    ArrowT a1 a2 ->
      (freeTypeVars a1 bound) ++ (freeTypeVars a2 bound) 
    PairT p1 p2 ->
      (freeTypeVars p1 bound) ++ (freeTypeVars p2 bound) 
    ForAllT tvar1 type1 ->
      freeTypeVars type1 (tvar1:bound)

-- Problem 3.
alphaRename :: TVar -> TVar -> Type -> Type
alphaRename vIn vOut ty = -- ty -- implement me!
  case ty of 
    BoolT -> BoolT
    NumT -> NumT
    StringT -> StringT
    VarT a -> if a == vIn then (VarT vOut)
              else (VarT a)
    ListT a1 ->
      (ListT (alphaRename vIn vOut a1))
    ArrowT a1 a2 ->
      (ArrowT (alphaRename vIn vOut a1) (alphaRename vIn vOut a2))
    PairT p1 p2 ->
      (PairT (alphaRename vIn vOut p1) (alphaRename vIn vOut p2))
    ForAllT tvar1 type1 ->
      if tvar1 == vIn then 
        (ForAllT vOut (alphaRename vIn vOut type1))
      else
        (ForAllT tvar1 (alphaRename vIn vOut type1))


-- Implementation complete: nothing to do here. Use this helper in checkType.
checkClosed :: Type -> [TVar] -> Result ()
checkClosed ty bound =
  case freeTypeVars ty bound of
   [] -> Ok ()
   nonEmpty -> Err ("unbound type var(s) in " ++ (show ty) ++ ": " ++ (show nonEmpty))

-- Problem 4.
-- freeTypeVars ty []
-- genFreshVar (allTypeVars type')
-- alphaRename x (genFreshVar (allTypeVars ty)) ty
subst :: TVar -> Type -> Type -> Type
subst var forType inType = -- inType -- implement me!
  case inType of 
    BoolT -> BoolT
    NumT -> NumT
    StringT -> StringT
    VarT vart ->
      if vart == var then forType
      else (VarT vart)
    ListT vart ->
      (ListT (subst var forType vart))
    ArrowT vart1 vart2 ->
      (ArrowT (subst var forType vart1) (subst var forType vart2))
    PairT vart1 vart2 ->
      (PairT (subst var forType vart1) (subst var forType vart2))
    ForAllT vart1 type1 ->
      if elem var (freeTypeVars (ForAllT vart1 type1) []) then
        case freeTypeVars forType [] of 
          [] -> (ForAllT vart1 (subst var forType type1))
          e:es -> 
            if elem e (allTypeVars (ForAllT vart1 type1)) then  
                (ForAllT 
                  (genFreshVar (allTypeVars (ForAllT vart1 type1)))
                  (subst var forType
                    (alphaRename e (genFreshVar (allTypeVars (ForAllT vart1 type1))) type1))
                  )
            else 
              (ForAllT vart1 (subst var forType type1))

      else
        (ForAllT vart1 type1)

-- A typing context includes a set of polymorphic type variables that
-- are in scope, along with bindings of program variables to their
-- declared types.
type TyContext = ([TVar], [(TVar, Type)])

-- Problem 5.
checkType :: DExpr -> TyContext -> Result Type
checkType expr gamma = -- Err "implement me!"
  case expr of
    VarD "+" -> Ok (NumT) 
    VarD "-" -> Ok (NumT) 
    VarD "*" -> Ok (NumT) 
    VarD "=" -> Ok (BoolT) 
    NumD num -> Ok (NumT)
    AppD fun arg ->
      case checkType fun gamma of
        Ok ty2 -> Ok ty2
    FunD var ty fun' ->
      case gamma of 
        ([], []) -> 
          case (checkType fun' gamma) of
            Ok ty1 -> Ok (ArrowT ty ty1)

-- Implementation complete.
-- Generates a variable name that's distinct from every name in the argument list.
-- Use this helper for alpha-renaming in subst.
genFreshVar :: [String] -> String
genFreshVar [] = "a"
genFreshVar tabu = reverse (inc (reverse (maximum tabu)))
  where inc ('z':cs) = 'a':'z':cs
        inc (c:cs) = (succ c):cs

-- Implementation complete: nothing to do here. Use this helper for alpha-renaming in subst.
allTypeVars :: Type -> [TVar]
allTypeVars (ArrowT t1 t2) = (allTypeVars t1) ++ (allTypeVars t2)
allTypeVars (ListT t1) = allTypeVars t1
allTypeVars (PairT t1 t2) = (allTypeVars t1) ++ (allTypeVars t2)
allTypeVars (VarT v) = [v]
allTypeVars (ForAllT v t) = [v] ++ (allTypeVars t)
allTypeVars NumT = []
allTypeVars BoolT = []
allTypeVars StringT = []

initialTypeEnv :: [(Var, Type)]
initialTypeEnv = [
  ("true", BoolT),
  ("false", BoolT),
  ("+", ArrowT NumT (ArrowT NumT NumT)),
  ("*", ArrowT NumT (ArrowT NumT NumT)),
  ("=", ArrowT NumT (ArrowT NumT BoolT)),
  ("<", ArrowT NumT (ArrowT NumT BoolT)),
  ("cons", ForAllT "a" (ArrowT (VarT "a")
                        (ArrowT (ListT (VarT "a")) (ListT (VarT "a"))))),
  ("empty", ForAllT "a" (ListT (VarT "a"))),
  ("first", ForAllT "a" (ArrowT (ListT (VarT "a")) (VarT "a"))),
  ("rest", ForAllT "a" (ArrowT (ListT (VarT "a")) (ListT (VarT "a")))),
  ("empty?", ForAllT "a" (ArrowT (ListT (VarT "a")) BoolT)),
  ("cons?", ForAllT "a" (ArrowT (ListT (VarT "a")) BoolT)),
  ("pair", ForAllT "a" (ForAllT "b" (ArrowT (VarT "a") (ArrowT (VarT "b")
                                                        (PairT (VarT "a") (VarT "b")))))),
  ("fst", ForAllT "a" (ForAllT "b" (ArrowT (PairT (VarT "a") (VarT "b"))
                                    (VarT "a")))),
  ("snd", ForAllT "a" (ForAllT "b" (ArrowT (PairT (VarT "a") (VarT "b"))
                                    (VarT "b")))),
  ("fix", ForAllT "a" (ArrowT (ArrowT (VarT "a") (VarT "a")) (VarT "a"))),
  ("call/cc", ForAllT "a" (ArrowT (ArrowT (ArrowT (VarT "a") (ForAllT "b" (VarT "b")))
                                   (VarT "a"))
                           (VarT "a")))
  ]

parseAndCheckStr :: String -> Result Type
parseAndCheckStr str =
  let toks = tokenize str in
  do (sexp, _) <- parseSExp toks
     expr <- parseExpr sexp
     cexp <- desugar expr
     checkType cexp ([], initialTypeEnv)
