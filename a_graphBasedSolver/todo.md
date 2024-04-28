* eq, gt, lt
* Enums
* CodeGen
	* inline
	* Closures: Welche Vars werden im Kontext benutzt?
* Record fields: Set Semantik
* Overloads / Resolution
* DateTime, TimeSpan
* Inheritance (double, int)
* Import .Net Methods and Types
* reorder Genvars (am Ende von Solve)

* Hier sieht man: Es muss einen Constraint von Lit(0) nach Fun(__) geben - nicht umgekehrt (siehe Codekommentar)
	let env9 = env [ ]
	App (Fun "__" (Var "__")) (Num 0.0)
	//|> Test.isOfType "unused fun field" env9 (stringTyp)
	|> showSolvedAst env9
* Inlining
	Beispiel:

	(Let "id" (Fun "x" (Record [ "whatever", Var "x" ] ))
	(Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ])
	)
	=
	...

