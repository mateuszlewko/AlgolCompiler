%% LEXER %% 
% TODO: Dopisać obsługe komentarzy

%% Na bazie while_parsera z KNO
lexer(Tokens) -->
   white_space,
   (  (  ":=",      !, { Token = tokAssgn }
	  ;  ";",       !, { Token = tokSColon }
	  ;  "(",       !, { Token = tokLParen }
	  ;  ")",       !, { Token = tokRParen }
	  ;  "+",       !, { Token = tokPlus }
	  ;  "-",       !, { Token = tokMinus }
	  ;  "*",       !, { Token = tokMult }
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
			;  Token = tokName(Id)
			%% ;  Token = tokProcedure(Id)
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

white_space --> [Char], { code_type(Char, space) }, !, white_space.
white_space --> [].

digit(D) --> [D], { code_type(D, digit) }.
digits([D|T]) --> digit(D), !, digits(T).
digits([]) --> [].

number(D, N) --> digits(Ds), { number_chars(N, [D | Ds]) }.

letter(L) --> [L], { code_type(L, alpha) }.

alphanum([A|T]) --> [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) --> [].

identifier(L, Id) --> alphanum(As), { atom_codes(Id, [L | As]) }.

%% PARSER %% 

/*
   SYNTAX ANALYSIS

   Context-free grammar:

	  program --> instruction | instruction program
	  instruction --> "while" bool_expr "do" program "done"
					| "if" bool_expr "then" program "else" program "fi"
					| "if" bool_expr "then" program "fi"
					| "skip" ";"
					| variable ":=" arith_expr ";"
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

%% :- op(990, xfy, ';;').
%% :- op(900, xfy, :=).
%% :- op(820, xfy, and).
%% :- op(840, xfy, or).
%% :- op(700, xfy, <=).
%% :- op(700, xfy, <>).

program(P) --> [tokProgram, tokName(Name)], block(P).

block(B) --> declarations(D), [tokBeg], complex_instruction(CI), [tokEnd], { B = blck(D, CI) }.

declarations(decl(D)) --> [], { D = [] }.
declarations(decl(D)) --> declaration(Dec), declarations(Decs), { append([Dec], Decs, D) }.
declaration(D) --> declarator(D).
declaration(D) --> procedure(D).
declarator(D) --> [tokLocal], variables(D).

%% vars(A, V, B) :- once(variables(A, V, B)). % przyda się pózniej
variables(V) --> variable(Var), { V = [Var]}.
variables(V) --> variable(Var), [tokComma], variables(Vars), { append([Var], Vars, V) }.
variable(var(V)) --> [tokName(V)].

procedure(P) --> [tokProcedure, tokName(Id)], [tokLParen], formal_args(FAs), [tokRParen], block(Bl), { P = proc(Id, FAs, Bl) }.

formal_args(FAs) --> [], { FAs = [] }.
formal_args(FAs) --> formal_args_series(FAs).
formal_args_series(FAS) --> formal_arg(FAS).
formal_args_series(FAS) --> formal_arg(FA), [tokComma], formal_args_series(FASs), { append(FA, FASs, FAS) }.
formal_arg(FA) --> [tokValue, tokName(Var)], { FA = [byValueArg(Var)] }.
formal_arg(FA) --> [tokName(Var)], { FA = [byNameArg(Var)] }. 

complex_instruction(CI) --> [], { CI = []}.
complex_instruction(CI) --> instruction(I), { CI = instr(I) }.
complex_instruction(CI) --> instruction(I), [tokSColon], complex_instruction(C), { CI = instr(I, C) }.

%% instruction(I) -->  (variable(Var), [tokAssgn], arith_expr(Expr), { I = assgn(Var, Expr) })
%% 					| ([tokIf], bool_expr(Bool), [tokThen], complex_instruction(CI), [tokFi], { I = if(Bool, CI)}).

%% program(Ast) -->
%%    instruction(Instr),
%%    (  program(Rest), !,
%% 		 { Ast = (Instr ';;' Rest) }
%%    ;  [],
%% 		 { Ast = Instr }
%%    ).

instruction(Instr) --> ([tokWhile], !, bool_expr(Bool), [tokDo], complex_instruction(Body), [tokDone], { Instr = while(Bool, Body) };
						[tokIf], !, bool_expr(Bool), [tokThen], complex_instruction(ThenPart),
							([tokElse], !, complex_instruction(ElsePart), [tokFi], { Instr = if(Bool, ThenPart, ElsePart) };  
							 [tokFi], { Instr = if(Bool, ThenPart) }
		 					);
   						variable(Var), !, [tokAssgn], arith_expr(Expr), { Instr = assgn(Var, Expr) };
   						[tokCall], !, procedure_call(PC), { Instr = proc_call(PC) };
   						[tokReturn], !, arith_expr(AE), { Instr = retr(AE) };
   						[tokRead], !, variable(Var), { Instr = read(Var) };
   						[tokWrite], !, arith_expr(AE), { Instr = write(AE) }
   						).

%% ;  [tokSkip], !, [tokSColon], { Instr = skip };

arith_expr(Expr) --> summand(Expr), !. % arith_expr(Summand, Expr).
arith_expr(Expr) --> arith_expr(E), additive_op(Op), summand(S), {Expr =.. [Op, E, S] }.
%% arith_expr(Acc, Expr) --> additive_op(Op), !, summand(Summand), { Acc1 =.. [Op, Acc, Summand] }, arith_expr(Acc1, Expr).
%% arith_expr(Acc, Acc) --> [].

summand(Expr) --> factor(Expr), !. %, summand(Factor, Expr).
summand(Acc, Expr) --> summand(Acc1, Expr), multiplicative_op(Op), !, factor(Factor), { Acc1 =.. [Op, Acc, Factor] }.
%% summand(Acc, Acc) --> [].

factor(Expr) --> simple_expr(Expr), !.% | ([tokMinus], simple_expr(-(Expr))).
simple_expr(Expr) --> atomic_expr(Expr), !.
simple_expr(Expr) --> [tokLParen], arith_expr(E), [tokRParen], { Expr = (E) }.

atomic_expr(atmic(Expr)) --> variable(Expr), !, { print(Expr), nl}.
atomic_expr(Expr) --> procedure_call(Expr), !.
atomic_expr(Expr) --> [tokNumber(N)], { print(N), nl, Expr = num(N) }.

procedure_call(PC) --> [tokName(Name), tokLParen], factual_args(FAs), [tokRParen], { PC = proc_call(Name, FAs) }.

factual_args(FAs) --> [].
factual_args(FAs) --> factual_args_series(FAs).

factual_args_series(FAS) --> factual_arg(FA), { FAS = [FA] }.
factual_args_series(FAS) --> factual_arg(FA), [tokComma], factual_args_series(FASs), { append([FA], FASs, FAS) }.

factual_arg(FA) --> arith_expr(FA).


   %% (  [tokLParen], !, arith_expr(Expr), [tokRParen]
   %% ;  [tokNumber(N)], !, { Expr = constant(N) }
   %% ;  [tokName(Var)], { Expr = variable(Var) }
   %% ).

bool_expr(B) --> conjunct(B), !.
bool_expr(B) --> bool_expr(B), [tokOr], conjunct(C), { B = or(B, C) }.

conjunct(C) --> condition(C), !.
conjunct(C) --> conjunct(Coj), [tokAnd], condition(Con), { C = and(Coj, Con) }.

condition(C) --> rel_expr(C).
condition(C) --> [tokNot], rel_expr(R), { C = not(R) }.

rel_expr(R) --> arith_expr(AL), rel_op(Rop), arith_expr(AR), { R =.. [Rop, AL, AR] }.
rel_expr((R)) --> [tokLParen], bool_expr(R), [tokRParen].

%% bool_expr(Bool) --> disjunct(Disjunct), bool_expr(Disjunct, Bool).

%% bool_expr(Acc, Bool) --> [tokOr], !, disjunct(Disjunct), { Acc1 =.. [or, Acc, Disjunct] }, bool_expr(Acc1, Bool).
%% bool_expr(Acc, Acc) --> [].

%% disjunct(Disjunct) --> conjunct(Conjunct), disjunct(Conjunct, Disjunct).

%% disjunct(Acc, Disjunct) --> [tokAnd], !, conjunct(Conjunct), { Acc1 =.. [and, Acc, Conjunct] }, disjunct(Acc1, Disjunct).
%% disjunct(Acc, Acc) --> [].

additive_op(+) --> [tokPlus], !.
additive_op(-) --> [tokMinus].

multiplicative_op(*) --> [tokMult], !.
multiplicative_op(div) --> [tokDiv], !.
multiplicative_op(mod) --> [tokMod].

%% conjunct(Conjunct) --
%%    (  [tokLParen], !, bool_expr(Conjunct), [tokRParen]
%%    ;  [tokNot], !, conjunct(NotConjunct),
%% 		 { Conjunct = not(NotConjunct) }
%%    ;  [tokTrue], !,
%% 		 { Conjunct = true }
%%    ;  [tokFalse], !,
%% 		 { Conjunct = false }
%%    ;  arith_expr(LExpr), rel_op(Op), arith_expr(RExpr),
%% 		 { Conjunct =.. [Op, LExpr, RExpr] }
%%    ).

rel_op(=) --> [tokEq], !.
rel_op(<>) --> [tokNeq], !.
rel_op(<) --> [tokLt], !.
rel_op(<=) --> [tokLeq], !.
rel_op(>) --> [tokGt], !.
rel_op(>=) -->[tokGeq].

parse(CharCodeList, Absynt) :-
   phrase(lexer(TokList), CharCodeList),
   phrase(program(Absynt), TokList).