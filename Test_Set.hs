interpVal(desugarVal(parseExprVal(parseSExp(tokenize "(with* ([f (fun (x) (+ x (raise "bad problem!")))] (+ 2 1))")))) initialEnv DoneK
interpVal(desugarVal(parseExprVal(parseSExp(tokenize "(with* ([f (fun (x) (+ x (raise \"bad\")))]) (+ 2 1))")))) initialEnv DoneK
interpVal(desugarVal(parseExprVal(parseSExp(tokenize "(with* ([f (fun (x) (+ x (raise \"bad\")))]) (call-with-handler (fun (_) (f 3)) (fun (err) (pair \"caught\" err))))")))) initialEnv DoneK
interpVal(desugarVal(parseExprVal(parseSExp(tokenize "(raise \"bad\")")))) initialEnv DoneK

(call-with-handler (fun (_) (f 3)) (fun (err) (pair "caught" err)))

(with* 
	(
		[f (
			fun (x) 
			(+ x (raise \"bad\"))
		)]
	) 
	(
		call-with-handler 
		(fun() (f 3)) 
		(fun (err) (pair \"caught\" err))
	)
)

interpVal(desugarVal(parseExprVal(parseSExp(tokenize "(+ (with* ([f (call-with-handler fun (_) (fun (x) (+ x (raise \"really bad problem!\")))(fun (err) (pair \"caught\" err)))]) (f 3)))")))) initialEnv DoneK

(+ 
	(with* 
		(
			[f 
				(call-with-handler 
					fun (_) (fun (x) (+ x (raise "really bad problem!")))
					(fun (err) (pair "caught" err))
				)
			]
		) 
		(f 3)
	)
)

(+ (with* ([f (call-with-handler fun (_) (fun (x) (+ x (raise "really bad problem!")))(fun (err) (pair "caught" err)))]) (f 3)))
