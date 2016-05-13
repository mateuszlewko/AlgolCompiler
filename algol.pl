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

program(P) --> [tokProgram, tokName(Name)], block(P, global).

block(B, Namespace) --> declarations(D, Namespace), [tokBeg], compound_instruction(CI, Namespace), [tokEnd], { B = blck(D, CI) }.

declarations(D, Namespace) --> [], { D = [] }.
declarations(decls(D)) --> declaration(Dec), declarations(Decs), { append(Dec, Decs, D) }.
declaration(D) --> declarator(D).
declaration(D) --> procedure(P), { D = [P]}.
declarator(D) --> [tokLocal], variables(D).

%% vars(A, V, B) :- once(variables(A, V, B)). % przyda się pózniej
variables(V) --> variable(Var), { V = [Var]}.
variables(V) --> variable(Var), [tokComma], variables(Vars), { append([Var], Vars, V) }.
variable(var(V)) --> [tokName(V)].

procedure(P) --> [tokProcedure, tokName(Id), tokLParen], formal_args(FAs), [tokRParen], block(Bl), { P = proc(Id, FAs, Bl) }.

formal_args(FAs) --> [], { FAs = [] }.
formal_args(FAs) --> formal_args_series(FAs).
formal_args_series(FAS) --> formal_arg(FAS).
formal_args_series(FAS) --> formal_arg(FA), [tokComma], formal_args_series(FASs), { append(FA, FASs, FAS) }.
formal_arg(FA) --> [tokValue, tokName(Var)], !, { FA = [byValueArg(Var)] }.
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
simple_expr(Expr) --> atomic_expr(Expr), !, { print("simple expr enter"), nl}.
simple_expr(Expr) --> [tokLParen], arith_expr(E), [tokRParen], { Expr = (E) }.

atomic_expr(Expr) --> procedure_call(Expr).
atomic_expr(Expr) --> variable(Expr), { print("atomic var enter"), nl, print(Expr), nl}.
atomic_expr(num(Expr)) --> [tokNumber(Expr)]. %, { Expr = num(N) }.

procedure_call(PC) --> [tokName(Name), tokLParen], factual_args(FAs), [tokRParen], 
	{ 	print("Proc call enter"), nl,
		PC = proc_call(Name, FAs), print(PC), nl }.

factual_args(FAs) --> [], { FAs = [], print("empty fac args enter"), nl}, !.
factual_args(FAs) --> factual_args_series(FAs).

factual_args_series(FAS) --> factual_arg(FA), !, { FAS = [FA] }.
factual_args_series(FAS) --> factual_arg(FA), [tokComma], factual_args_series(FASs), { append([FA], FASs, FAS) }.

factual_arg(FA) --> arith_expr(FA).

bool_expr(B) --> conjunct(B), !.
bool_expr(B) --> conjunct(C), [tokOr], bool_expr(Be), { B = or(C, Be) }.

conjunct(C) --> condition(C), !.
conjunct(C) --> condition(Con), [tokAnd], conjunct(Coj), { C = not(or(not(Con), not(Coj))) }.

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

stringCompile(String, Code, Assemb) :-
	string_codes(String, CharList),
	parse(CharList, Absynt),
	assembler(Absynt, Code, Assemb).

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

%% ASSEMBLER %%

lookupCmdCode(Cmd, Code) :- 
	Code = d{nop:0, syscall:1, load:2, store:3, swapa:4, swapd:5,
			branchz:6, branchn:7, jump:8, const:9, add:10, sub:11, mul:12, div:13}.get(Cmd).

setJumpPos([], Pos).

setJumpPos([currPos(Cmd, X) | Rest], Pos) :-
	var(Cmd), !,
	Cmd is Pos,
	NextPos = Pos + 1,
	setJumpPos(Rest, NextPos).

setJumpPos([Cmd | Rest], Pos) :- 
	nonvar(Cmd),
	NextPos = Pos + 1, 
	setJumpPos(Rest, NextPos).

%% dictLookup(Dict, Key, Res) :-
%% 	print("dict:"), nl, print(Dict), nl, print(Key), nl,
%% 	Res = Dict.get(Key).
addToDict(Dict, Key, Value, Dict.put(Key, Value)).

setVarAdresses([], [], D, Cnt).
setVarAdresses([var(V) | Rest], [Var | Result], VarsDict, Cnt) :- !,
	(Var = VarsDict.get(V), NextVarsDict = VarsDict, NextCnt = Cnt;
	Var is 2^16 - Cnt - 1, addToDict(VarsDict, V, Var, NextVarsDict), NextCnt is Cnt + 1), !,
	setVarAdresses(Rest, Result, NextVarsDict, NextCnt).

setVarAdresses([Cmd | Rest], [Cmd | Result], VarsDict, Cnt) :- setVarAdresses(Rest, Result, VarsDict, Cnt).

setSymbols([], [], Pos).
setSymbols([currPos(N) | Rest], [Val | Result], Pos) :-
	Val is Pos + N,
	NextPos = Pos + 1, !,
	setSymbols(Rest, Result, NextPos).
setSymbols([currPos(Curr, N) | Rest], [Val | Result], Pos) :-
	Val is Curr + N,
	NextPos = Pos + 1, !,
	setSymbols(Rest, Result, NextPos).

setSymbols([Num | Rest], [Num | Result], Pos) :-
	number(Num), !,
	NextPos = Pos + 1,
	setSymbols(Rest, Result, NextPos).

setSymbols([Cmd | Rest], [CmdCode | Result], Pos) :-
	NextPos = Pos + 1, !,
	lookupCmdCode(Cmd, CmdCode),
	setSymbols(Rest, Result, NextPos).

%% setSymbols([Cmd | Rest], [CmdCode | Result], Pos) :-
%% 	NextPos = Pos + 1, !,
%% 	\+ lookupCmdCode(Cmd, CmdCode),
%% 	setSymbols(Rest, Result, NextPos).

assembler(Prog, Code, NonHex) :-
	encode(Prog, Code),
	setJumpPos(Code, 0),
	setVarAdresses(Code, VarsWithAdresses, d{}, 0),
	setSymbols(VarsWithAdresses, NonHex, 0).


%% ENKODER %%

%% POMOCNICZE %%

% Returns negative number in Alogol16
negativeNumber(X, N) :- N is 2^16 - X.

% Przenieś ACC do DR,
% Załaduj currPos + 10 do ACC, przenieś do AR
% powróć z wynikiem z DR do ACC,
% skocz do ustawienia ACC na -1 jeśli w ACC jest 0
% jeśli w ACC nie ma 0 to ustaw ACC na 0 (przenieś do DR) i przeskocz ustawianie na -1
% jeśli trzeba ustaw ACC na -1 i przenieś do DR
% powróć z wynikiem z DR do ACC (wspólne dla obu warunków)
negateACC(Code) :-
	negativeNumber(1, MinusOne),
	Code = [swapd, const, currPos(10), swapa, swapd, branchz, const, 0, swapd, const, 
			currPos(5), jump, const, MinusOne, swapd, swapd].


encode(blck(Declarations, Instructions), Code) :- 
	encode(Instructions, BlckCode),
	append(BlckCode, [const, 0, syscall], Code).
	
encode([], []) :- !.
encode([Instruction | Rest], Code) :- 
	encode(Instruction, Cmd),
	encode(Rest, Cmds),
	append(Cmd, Cmds, Code).

% predykat zapisuje ACC do tymczasowej komórki i ją przeskakuje
storeAccToTemp([swapd, const, currPos(VarPos, 7), swapa, swapd, store, const, currPos(3), jump, nop], currPos(VarPos, 7)). 


%% WYRAŻENIA ATOMOWE %%

% załaduj liczbę do ACC
encode(num(N), [const, N]).

% załaduj adres zmiennej do ACC
% przenieś adres z ACC do AR
% załaduj zmienną (LOAD)
encode(var(V), [const, var(V), swapa, load]).


%% INSTRUKCJE %%

encode(instr(I), Code) :- encode(I, Code).

% policz prawą stronę wyrazenia
% wynik musi być w ACC
% przenieś wynik z ACC do DR
% załaduj adres zmiennej do ACC
% przenieś adres z ACC do AR
% przenieś wynik z DR do ACC
% zapisz ACC do komórki nr z AR
encode(assgn(V, Expr), Code) :- 
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
encode(read(V), Code) :- 
	Code = [const, 1, syscall, swapd, const, V, swapa, swapd, store].

% załaduj adres zmiennej do ACC
% przenieś adres do AR
% załaduj zmienną do ACC
% przenieś zmienną z ACC do DR
% załaduj 2 do ACC (kod SYSCALL::WRITE)
% SYSCALL (write)
encode(write(V), Code) :- 
	Code = [const, V, swapa, load, swapd, const, 2, syscall].

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
	length(CondCmds, CondCnt), length(InstrCmds, InstrCnt),
	append(CondCmds, [swapd, const, currPos(InstrCnt + 3 + 3 + 1), swapa, swapd, branchz], Cmds),
	
	length(Cmds, CmdsCnt),
	append(InstrCmds, [const, currPos(-InstrCnt - CmdsCnt - 1), jump], EndingCmds),
	append(Cmds, EndingCmds, Code).

encode(if(Bool, ThenPart, ElsePart), Code) :-  !,
	encode(Bool, BoolTempCmds),
	encode(ElsePart, ElseCmds),
	length(ElseCmds, ElseCnt),
	
	encode(ThenPart, ThenTempCmds),
	append(ThenTempCmds, [const, currPos(2 + ElseCnt), jump], ThenCmds),
	length(ThenCmds, ThenCnt),

	append(BoolTempCmds, [swapd, const, currPos(4 + ThenCnt), swapa, swapd, branchz], BoolCmds),
	append(BoolCmds, [ThenPart, ElsePart], TempCode),
	append(TempCode, Code).

encode(if(Bool, ThenPart), Code) :-
	encode(Bool, BoolTempCmds),	
	encode(ThenPart, ThenTempCmds),
	length(ThenCmds, ThenCnt),
	append(BoolTempCmds, [swapd, const, currPos(4 + ThenCnt), swapa, swapd, branchz], BoolCmds),
	append(BoolCmds, ThenPart, Code).


%% OPERATORY RELACYJNE %%

% Oblicz L - R w ACC
% jeśli wynik jest równy 0 to go pozostaw w ACC ( 0 == false)
% i przeskocz ustawianie ACC na -1 ( -1 == true)
encode(<>(L, R), Code) :- 
	arithm_encode(L, R, sub, Cmds),
	negativeNumber(1, MinusOne),
	append(Cmds, [swapd, const, currPos(6), swapa, swapd, branchz, const, MinusOne], Code).

% Oblicz L <> R, zaneguj ACC
encode(=(L, R), Code) :-
	encode(<>(L, R), Cmds),
	negateACC(Neg),
	append(Cmds, Neg, Code).
	%% arithm_encode(L, R, sub, Cmds),
	%% negativeNumber(1, MinusOne),
	%% append(Cmds, [swapd, const, currPos(10), swapa, swapd, branchz, const, 0, swapd, const, currPos(5), jump,
	%% 					const, MinusOne, swapd, swapd], Code).

% Oblicz L - R,
% Jeśli ACC < 0 to przeskocz ustawianie ACC na 0 (ACC < 0 to true)
encode(<(L, R), Code) :-
	arithm_encode(L, R, sub, Cmds),
	negativeNumber(1, MinusOne),
	append(Cmds, [swapd, const, currPos(6), swapa, swapd, branchn, const, 0], Code).

encode(>(L, R), Code) :- encode(<(R, L), Code).
encode(<=(L, R), Code) :-
	encode(>(L, R), Cmds),
	negateACC(Neg),
	append(Cmds, Neg, Code).

encode(>=(L, R), Code) :- encode(<=(R, L), Code).


%% OPERATORY ARYTMETYCZNE %%

% enkoduj lewą stronę, wynik przenieś z ACC do currPos(VarPos)
% przeskocz currPos(VarPos)
% enkoduj prawą stronę, wynik przenieś z ACC do DR
% załaduj currPos(VarPos) do ACC
% wykonaj operacje arytm DR do ACC
arithm_encode(L, R, Cmd, Code) :-
	encode(L, LCmdsTemp),
	storeAccToTemp(StoreCmds, VarPos),
	append(LCmdsTemp, StoreCmds, LCmds),
	encode(R, RCmdsTemp),
	append(RCmdsTemp, [swapd, const, VarPos, swapa, load, Cmd], RCmds),
	append(LCmds, RCmds, Code).

%% TODO: Operator modulo (mod(L, R))
encode(L + R, Code) :- arithm_encode(L, R, add, Code).
encode(L - R, Code) :- arithm_encode(L, R, sub, Code).
encode(div(L, R), Code) :- arithm_encode(L, R, div, Code).
encode(L * R, Code) :- arithm_encode(L, R, mul, Code).


%% OPERATORY LOGICZNE %%

% enkoduj wyrazenie, wynik jest w ACC
% zaneguj ACC
encode(not(Expr), Code) :-
	encode(Expr, Cmds),
	negateACC(Neg),
	append(Cmds, Neg, Code).

encode(or(L, R), Code) :- encode(<((L + R), num(0)), Code).
% nie będzie 'and' do enkodowania bo: L and R jest zamieniane w parserze na not(not(L) or not(R))

