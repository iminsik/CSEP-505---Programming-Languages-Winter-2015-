{-
x = 1

myDrop n xs = if n <= 0 || null xs then xs else myDrop (n - 1) (tail xs)

-- mydrop :: Integer -> [a]

--mydrop [x:xs] = xs
mydrop :: Integer -> [a] -> [a]
mydrop _ [] = []
mydrop n s@(x:xs) | n < 1 = (x:xs)
                | otherwise = mydrop (pred n) xs

-- -----------------------------------------

-- Substitution -> inline function
-}

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]

-- 0 = \sz . z 
-- Church Encoding

-- y f x = f (y f) x
-- Y Combinator: incefficient
-- http://stackoverflow.com/questions/7141229/how-to-write-recursive-lambda-expression-in-haskell
-- http://stackoverflow.com/questions/4273413/y-combinator-in-haskell
data Paradox a = Self (Paradox a -> a)
fixpoint f = let half (Self twin) = f (twin (Self twin))
             in half (Self half)
factorial = fixpoint (\ff n -> if n == 1 then 1 else n * ff(n-1))

data Point a = Point a a deriving Show

type MyString             = [Char]
type Person             = (Name,Address)
type Name               = MyString
data Address            = None | Addr MyString

data MyChar a = Ca a | Cb a

quicksort [] = []
quicksort (x:xs) 	= quicksort [y | y <- xs, y < x]
					++ [x]
					++ quicksort [y | y <- xs, y >= x]

myinc	= \x -> x + 1

-- Continuation Passing Style: let/cc
-- Store Passing
-- Box
-- Environment
-- Store Transformer

-- PrimV ????
-- Infinite set
-- Relation: e | v , saying e evalutes v. (e, v) -> Relation

-- Big Step Semantics, Natural Deduction
-- Small Step Semantics