(+ (call/cc 
	(lambda (k)
		(k (* 3 4))))
	5 )

(+ (* 3 4) 5)	-- k: (lambda (v) v)
(* 3 4)			-- k: (lambda (v) (+ v 5))
3				-- k: (lambda (v) (+ (* v 4) 5))
4				-- k: (lambda (v) (+ (* 3 v) 5))
5				-- k: (lambda (v) (+ (* 3 4) v))
