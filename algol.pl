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
%% TODO: Łączenie operatorów w lewo (teraz łączy w prawo)

program(P) --> [tokProgram, tokName(Name)], block(P).

block(B) --> declarations(D), [tokBeg], compound_instruction(CI), [tokEnd], { B = blck(D, CI) }.

declarations(D) --> [], { D = [] }.
declarations(decls(D)) --> declaration(Dec), declarations(Decs), { append(Dec, Decs, D) }.
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

%% compound_instruction(CI) --> [], { CI = []}.
compound_instruction(CI) --> instruction(I), { CI = [I] }.
compound_instruction(CI) --> instruction(I), [tokSColon], compound_instruction(C), { append([I], C, CI) }. %{ CI = instr(I, C) }.

instruction(Instr) --> ([tokWhile], !, bool_expr(Bool), [tokDo], compound_instruction(Body), [tokDone], { Instr = while(Bool, Body) };
						[tokIf], !, bool_expr(Bool), [tokThen], compound_instruction(ThenPart),
							([tokElse], !, compound_instruction(ElsePart), [tokFi], { Instr = if(Bool, ThenPart, ElsePart) };  
							 [tokFi], { Instr = if(Bool, ThenPart) }
		 					);
   						variable(Var), !, [tokAssgn], arith_expr(Expr), { Instr = assgn(Var, Expr) };
   						[tokCall], !, procedure_call(PC), { Instr = proc_call(PC) };
   						[tokReturn], !, arith_expr(AE), { Instr = retr(AE) };
   						[tokRead], !, variable(Var), { Instr = read(Var) };
   						[tokWrite], !, arith_expr(AE), { Instr = write(AE) }
   						).

arith_expr(Expr) --> summand(Expr).
arith_expr(Expr) --> summand(S), additive_op(Op), arith_expr(E), {Expr =.. [Op, S, E] }.

summand(Expr) --> factor(Expr).
summand(Expr) --> factor(Factor), multiplicative_op(Op), !, summand(Summ), { Expr =.. [Op, Factor, Summ] }.
%% summand(Acc, Expr) --> summand(Acc1, Expr), multiplicative_op(Op), !, factor(Factor), { Acc1 =.. [Op, Acc, Factor] }.

factor(Expr) --> simple_expr(Expr), !.
factor(Expr) --> [tokMinus], simple_expr(-(Expr)).
simple_expr(Expr) --> atomic_expr(Expr), !.
simple_expr(Expr) --> [tokLParen], arith_expr(E), [tokRParen], { Expr = (E) }.

atomic_expr(Expr) --> variable(Expr), !.%, { print(Expr), nl}.
atomic_expr(Expr) --> procedure_call(Expr), !.
atomic_expr(num(Expr)) --> [tokNumber(Expr)]. %, { Expr = num(N) }.

procedure_call(PC) --> [tokName(Name), tokLParen], factual_args(FAs), [tokRParen], { PC = proc_call(Name, FAs) }.

factual_args(FAs) --> [].
factual_args(FAs) --> factual_args_series(FAs).

factual_args_series(FAS) --> factual_arg(FA), { FAS = [FA] }.
factual_args_series(FAS) --> factual_arg(FA), [tokComma], factual_args_series(FASs), { append([FA], FASs, FAS) }.

factual_arg(FA) --> arith_expr(FA).

bool_expr(B) --> conjunct(B), !.
bool_expr(B) --> bool_expr(B), [tokOr], conjunct(C), { B = or(B, C) }.

conjunct(C) --> condition(C), !.
conjunct(C) --> conjunct(Coj), [tokAnd], condition(Con), { C = and(Coj, Con) }.

condition(C) --> rel_expr(C), !.
condition(C) --> [tokNot], rel_expr(R), { C = not(R) }.

rel_expr(R) --> arith_expr(AL), rel_op(Rop), arith_expr(AR), { R =.. [Rop, AL, AR] }.
rel_expr((R)) --> [tokLParen], bool_expr(R), [tokRParen].

additive_op(+) --> [tokPlus], !.
additive_op(-) --> [tokMinus].

multiplicative_op(*) --> [tokMult], !.
multiplicative_op(div) --> [tokDiv], !.
multiplicative_op(mod) --> [tokMod].

rel_op(=) --> [tokEq], !.
rel_op(<>) --> [tokNeq], !.
rel_op(<) --> [tokLt], !.
rel_op(<=) --> [tokLeq], !.
rel_op(>) --> [tokGt], !.
rel_op(>=) -->[tokGeq].

parse(CharCodeList, Absynt) :-
   phrase(lexer(TokList), CharCodeList),
   phrase(program(Absynt), TokList).

%% encode(CharCodeList, Code) :- 
%% 	phrase(lexer(TokList), CharCodeList),
%%    	phrase(program(Absynt), TokList),
%%    	print("encode0, absynt:"), nl,
%%    	print(Absynt), nl,
%%    	encode(Absynt, Code).


%% encode(Program, Code) :-
%% 	encode(Program, Code, Index), !.

encode(blck(Declarations, Instructions), Code) :- 
	%% print("encode1"), nl,
	%% print(Instructions), nl,
	encode(Instructions, Code).
	%% print("Index:"), nl,
	%% print(Index), nl, !.

encode([], []) :- !.
encode([Instruction | Rest], Code) :- 
	%% print("encode2"), nl,
	encode(Instruction, Cmd),
	encode(Rest, Cmds),
	append(Cmd, Cmds, Code).

% załaduj liczbę do ACC
encode(num(N), Code) :-
	Code = [const, N].

% załaduj adres zmiennej do ACC
% przenieś adres z ACC do AR
% załaduj zmienną (LOAD)
encode(var(V), Code) :-
	Code = [const, V, swapa, load].


% policz prawą stronę wyrazenia
% wynik musi być w ACC
% przenieś wynik z ACC do DR
% załaduj adres zmiennej do ACC
% przenieś adres z ACC do AR
% przenieś wynik z DR do ACC
% zapisz ACC do komórki nr z AR
encode(assgn(var(V), Expr), Code) :- 
	encode(Expr, ExprCmds),
	Cmds = [swapd, const, V, swapa, swapd, store],
	append(ExprCmds, Cmds, Code).

% załaduj 1 do ACC (kod SYSCALL::READ)
% SYSCALL (read)
% przenieś wczytane słowo z ACC do DR
% załaduj adres zmiennej do ACC
% przenieś adres zmiennej z ACC do AR
% przenieś słowo z DR do ACC
% zapisz słowo (ACC) do komórki nr z AR
encode(read(var(V)), Code) :- 
	Code = [const, 1, syscall, swapd, const, V, swapa, swapd, store].

% enkoduj instrukcje warunku: w akumulatorze jest -1 jeśli warunek jest prawdziwy 
% lub 0 jeśli jest fałszywy 
% przenieś wynik z ACC do DR
% załaduj adres pierwszej komórki pamięci po zakończeniu pętli: current_pos + liczba_intrukcji_w_pętli + 3 + 3 + 1
% skocz poza pętle jeśli w ACC jest 0 (BRANCHZ)
% enkoduj instrukcje w pętli
% skocz do komórki pamięci current_pos - (liczba_instrukcji_w_pętli + 
% 									  + liczba_instrukcji_w_warunku + 1)
encode(while(Cond, Instrs), Code) :- 
	encode(Cond, CondCmds), encode(Instrs, InstrCmds),
	%% print("InstrCmds:"), nl, print(InstrCmds), nl,
	length(CondCmds, CondCnt), length(InstrCmds, InstrCnt),
	append(CondCmds, [swapd, const, currPos(InstrCnt + 3 + 3 + 1), swapa, swapd, branchz], Cmds),
	length(Cmds, CmdsCnt),
	append(InstrCmds, [const, currPos(-InstrCnt - CmdsCnt - 1), jump], EndingCmds),
	append(Cmds, EndingCmds, Code).
	%% append([while], TempCmds, Code).

% enkoduj prawą strone wyrazenia
% wynik znajduje się w ACC, przenieś do DR
% enkoduj lewą stronę, wynik jest w ACC
% odejmij DR od ACC (SUB)
% przenieś wynik z ACC od DR
% załaduj adres komórki + 6 i przenieś do AR
% powróć z wynikiem z DR do ACC
% skocz jeśli ACC równy 0 (omiń ustawienie ACC na -1 / true)
% ACC jest juz 0 (jeśli jest false)
encode(<>(L, R), Code) :- 
	encode(R, RCmdsTemp),
	append(RCmdsTemp, [swapd], RCmds),
	encode(L, LCmdsTemp),
	append(LCmdsTemp, [sub], LCmds),
	append(RCmds, LCmds, Cmds),
	append(Cmds, [swapd, const, currPos(6), swapa, swapd, branchz, const, -1], Code).

encode(instr(I), Code) :- encode(I, Code).

% załaduj adres zmiennej do ACC
% przenieś adres do AR
% załaduj zmienną do ACC
% przenieś zmienną z ACC do DR
% załaduj 2 do ACC (kod SYSCALL::WRITE)
% SYSCALL (write)
encode(write(V), Code) :- 
	Code = [const, V, swapa, load, swapd, const, 2, syscall].



