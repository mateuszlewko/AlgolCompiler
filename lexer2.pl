%% :- module(while_parser, [parse/2]).

lexer(Tokens) -->
   white_space,
   (  (  ":=",      !, { Token = tokAssgn }
	  ;  ";",       !, { Token = tokSColon }
	  ;  "(",       !, { Token = tokLParen }
	  ;  ")",       !, { Token = tokRParen }
	  ;  "+",       !, { Token = tokPlus }
	  ;  "-",       !, { Token = tokMinus }
	  ;  "*",       !, { Token = tokTimes }
	  ;  "=",       !, { Token = tokEq }
	  ;  "<>",      !, { Token = tokNeq }
	  ;  "<=",      !, { Token = tokLeq }
	  ;  "<",       !, { Token = tokLt }
	  ;  ">=",      !, { Token = tokGeq }
	  ;  ">",       !, { Token = tokGt }
	  ;  ",", 		!, { Token = tokComma} 
	  ;  digit(D),  !,
			number(D, N),
			{ Token = tokNumber(N) }
	  ;  letter(L), !, identifier(L, Id),
			{  member((Id, Token), [ (and, tokAnd),
									 (begin, tokBeg),
									 (call, tokCall),								
									 (div, tokDiv),
									 (do, tokDo),
									 (done, tokDone),
									 (else, tokElse),
									 (end, tokEnd),
									 (fi, tokFi),
									 (if, tokIf),
									 (local, tokLocal),
									 (mod, tokMod),
									 (not, tokNot),
									 (or, tokOr),
									 (procedure, tokProcedure),
									 (program, tokProgram),
									 (read, tokRead),
									 (return, tokReturn),
									 (then, tokThen),
									 (value, tokValue),
									 (while, tokWhile),
									 (write, tokWrite)]),
			   !
			;  Token = tokVar(Id)
			}
	  ;  [_],
			{ Token = tokUnknown }
	  ),
	  !,
		 { Tokens = [Token | TokList] },
	  lexer(TokList)
   ;  [],
		 { Tokens = [] }
   ).

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].
   
digit(D) -->
   [D],
	  { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
	  { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
	  { atom_codes(Id, [L|As]) }.

/*
   SYNTAX ANALYSIS

   Context-free grammar:

	  program --> instruction | instruction program
	  instruction --> "while" bool_expr "do" program "done"
					| "if" bool_expr "then" program "else" program "fi"
					| "if" bool_expr "then" program "fi"
					| "skip" ";"
					| variable ":=" artith_expr ";"
	  arith_expr --> arith_expr additive_op summand | summand
	  summand --> summand multiplicative_op factor | factor
	  factor --> "(" arith_expr ")" | constant | variable
	  additive_op --> "+" | "-"
	  multiplicative_op --> "*" | "div" | "mod"
	  bool_expr --> bool_expr "or" disjunct | disjunct
	  disjunct --> disjunct "and" conjunct | conjunct
	  conjunct --> "(" bool_expr ")" | "not" conjunct | "true" | "false"
				 | arith_expr rel_op arith_expr
	  rel_op --> "=" | "<>" | "<" | "<=" | ">" | ">="

   To get a complete parser it suffices to replace character terminals
   in the grammar above with lexical tokens, eliminate left recursion and
   add appropriate semantic actions generating abstract syntax trees.
*/

:- op(990, xfy, ';;').
:- op(900, xfy, :=).
:- op(820, xfy, and).
:- op(840, xfy, or).
:- op(700, xfy, <=).
:- op(700, xfy, <>).

program(Ast) -->
   instruction(Instr),
   (  program(Rest), !,
		 { Ast = (Instr ';;' Rest) }
   ;  [],
		 { Ast = Instr }
   ).

instruction(Instr) -->
   (  [tokWhile], !, bool_expr(Bool), [tokDo], program(Body), [tokDone],
		  { Instr = while(Bool, Body) }
   ;  [tokIf], !, bool_expr(Bool), [tokThen], program(ThenPart),
		 (  [tokElse], !, program(ElsePart), [tokFi],
			   { Instr = if(Bool, ThenPart, ElsePart) }
		 ;  [tokFi],
			   { Instr = if(Bool, ThenPart) }
		 )
   ;  [tokSkip], !, [tokSColon],
		 { Instr = skip }
   ;  [tokVar(Var), tokAssgn], arith_expr(Expr), [tokSColon],
		 { Instr = (Var := Expr) }
   ).

arith_expr(Expr) -->
   summand(Summand), arith_expr(Summand, Expr).

arith_expr(Acc, Expr) -->
   additive_op(Op), !, summand(Summand),
	  { Acc1 =.. [Op, Acc, Summand] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) -->
   [].

summand(Expr) -->
   factor(Factor), summand(Factor, Expr).

summand(Acc, Expr) -->
   multiplicative_op(Op), !, factor(Factor),
	  { Acc1 =.. [Op, Acc, Factor] }, summand(Acc1, Expr).
summand(Acc, Acc) -->
   [].

factor(Expr) -->
   (  [tokLParen], !, arith_expr(Expr), [tokRParen]
   ;  [tokNumber(N)], !, { Expr = constant(N) }
   ;  [tokVar(Var)], { Expr = variable(Var) }
   ).

bool_expr(Bool) -->
   disjunct(Disjunct), bool_expr(Disjunct, Bool).

bool_expr(Acc, Bool) -->
   [tokOr], !, disjunct(Disjunct),
	  { Acc1 =.. [or, Acc, Disjunct] }, bool_expr(Acc1, Bool).
bool_expr(Acc, Acc) -->
   [].

disjunct(Disjunct) -->
   conjunct(Conjunct), disjunct(Conjunct, Disjunct).

disjunct(Acc, Disjunct) -->
   [tokAnd], !, conjunct(Conjunct),
	  { Acc1 =.. [and, Acc, Conjunct] }, disjunct(Acc1, Disjunct).
disjunct(Acc, Acc) -->
   [].

conjunct(Conjunct) -->
   (  [tokLParen], !, bool_expr(Conjunct), [tokRParen]
   ;  [tokNot], !, conjunct(NotConjunct),
		 { Conjunct = not(NotConjunct) }
   ;  [tokTrue], !,
		 { Conjunct = true }
   ;  [tokFalse], !,
		 { Conjunct = false }
   ;  arith_expr(LExpr), rel_op(Op), arith_expr(RExpr),
		 { Conjunct =.. [Op, LExpr, RExpr] }
   ).

additive_op(+) -->
   [tokPlus], !.
additive_op(-) -->
   [tokMinus].

multiplicative_op(*) -->
   [tokTimes], !.
multiplicative_op(//) -->
   [tokDiv], !.
multiplicative_op(mod) -->
   [tokMod].

rel_op(=) -->
   [tokEq], !.
rel_op(<>) -->
   [tokNeq], !.
rel_op(<) -->
   [tokLt], !.
rel_op(<=) -->
   [tokLeq], !.
rel_op(>) -->
   [tokGt], !.
rel_op(>=) -->
   [tokGeq].

parse(CharCodeList, Absynt) :-
   phrase(lexer(TokList), CharCodeList),
   phrase(program(Absynt), TokList).