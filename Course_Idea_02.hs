type Var = String

-- Stash Test

(with [x (+ 1 2)]
	(+ x (with [x (* x x)]
		(+ x x))))

{-
(with [a (+ 1 2)]
	(+ a (with [b (* a a)]
		(+ b b))))
-}

NumE n -> expr
WithE var boundExpr body ->
	interp(subst var (interp boundExpr) body)

subst :: Var -> Expr -> Expr -> Expr
subst var boundExpr body =
	let recur = subst var boundExpr in
	case body of
		NumE n  -> Num n
		OpE op lhs rhs = 
			OpE op (recur lhs) (recur rhs) 
		VarE var' | var == var' -> boundExpr
		| otherwise -> body
		WithE var' boundExpr' body' | var == var' ->
			withE var' (recur boundExpr) -> body'
			| otherwise -> withE var' (recur boundExpr) (recur body')

VarE var -> error (var ++ ": unbound id")

-- Concept #1
-- This expressions has a BUG.

(with [x (+ y 2)]
	(with [y 4]
		(+ x y)))

-- Actually it results 'Captured Variable'
(with [x (+ y 2)]
	(with [y' 4]
		(+ x y')))


-- Concept #2
(with [sqr (fun (x) (* x x))]
	(+ (* 3 3) (* 4 4)))

-- becomes as below.
-- fun is Lambda actually.
(with [sqr (fun (x) (* x x))]
	(+ (sqr 3 3) (sqr 4 4)))

-- Lambda function call
((fun(x) (* x x))
	(+ 1 2))

((fun (f) (f (f 3)))
	(fun (x) (+ x 2)))
{-
((fun (f) (f (f 3)))
	(fun (x) (+ x 2)))
-}
((fun (fun (x)) (f (f 3)))
	(fun (x) (+ x 2)))

-- becomes
((fun (x) 
	(fun (f) (f (f x))))
2)

-- What is curry????

-- Lambda
(\ x y -> x + y) 3 4
(\f -> (f (f 3))) (\x -> x + 2)
(\f y -> (f (f y))) (\x -> x + 2) 4

-- New term: De-sugaring - oppose to 'Syntactic Sugar'
(with [x 2]
	(with [y 3]
		(+ (* (+ 3 4) (+ 1 2))
			(+ (* 2 x) (* y 3)))))

-- Substitution is a key of this class
-- AppE is a lambda function.

-- What's haskell 'lookup' function?

(with [y 2]
-- [y -> 2]
	(with [f (fun(x) (+ x y))]
				-- x -> 5, y -> 5, f -> fun(x) ..., y -> 2
				-- It should be y -> 2!!!!
-- [f -> fun(x) ..., y -> 2]
		(with [y 5]
-- [y -> 5, f -> fun(x) ..., y -> 2]: a kind of STACK to find the last
			(f y)))

-- implement 'Closure'
data Val = NumV Int
		| BoolV Bool
		| FunV Var Expr env

-- Function should not substitute more
-- Static variable
FunV var body closEnd ->
	interp body (var, interp arg env): closure

-- Even though the implementation is EAGER, haskell will interprete it LAZY.

(with [f (fun (x) (* x x))]
	(f (+ 1 2)))

let ones = 1:ones
:t ones
:t take
take 5 ones

let foo = scan1 (+) 0 ones
take 20 foo

let fib = 1:1:(zipWith (+) fib (tail fib))
:t fib
take 5 fib
take 30 fib
