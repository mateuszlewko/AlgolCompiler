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

my_atom_concat(Prev, Name, Next) :- atomic_list_concat([Prev, Name], '|', Next).

program(P) --> [tokProgram, tokName(Name)], block(P, global).

block(B, Namespace) --> declarations(D, Namespace), [tokBeg], compound_instruction(CI, Namespace), [tokEnd], { B = blck(D, CI) }.

declarations(D, Namespace) --> [], { D = [] }.
declarations(D, Namespace) --> declaration(Dec, Namespace), declarations(Decs, Namespace), { append(Dec, Decs, D) }.
declaration(D, Namespace) --> declarator(D, Namespace).
declaration(D, Namespace) --> procedure(P, Namespace), { D = [P]}.
declarator(D, Namespace) --> [tokLocal], variables(D, Namespace).

%% vars(A, V, B) :- once(variables(A, V, B)). % przyda się pózniej
variables(V, Namespace) --> variable(Var, Namespace), { V = [Var]}.
variables(V, Namespace) --> variable(Var, Namespace), [tokComma], variables(Vars, Namespace), { append([Var], Vars, V) }.
variable(var(V), Namespace) --> [tokName(VName)], { my_atom_concat(Namespace, VName, V) }.

procedure(P, Namespace) --> [tokProcedure, tokName(Id), tokLParen], { my_atom_concat(Namespace, Id, NextNamespace) },
							formal_args(FAs, NextNamespace), [tokRParen], block(Bl, NextNamespace), 
							{ P = proc(NextNamespace, FAs, Bl) }.

formal_args(FAs, Namespace) --> [], { FAs = [] }.
formal_args(FAs, Namespace) --> formal_args_series(FAs, Namespace).

formal_args_series(FAS, Namespace) --> formal_arg(FAS, Namespace).
formal_args_series(FAS, Namespace) --> formal_arg(FA, Namespace), [tokComma], formal_args_series(FASs, Namespace), 
										{ append(FA, FASs, FAS) }.

formal_arg(FA, Namespace) --> [tokValue, tokName(Var)], !, 
								{ my_atom_concat(Namespace, Var, V), FA = [byValueArg(V)] }.
formal_arg(FA, Namespace) --> [tokName(Var)], 
								{ my_atom_concat(Namespace, Var, V), FA = [byNameArg(V)] }. 

%% compound_instruction(CI) --> [], { CI = []}.
compound_instruction(CI, Namespace) --> instruction(I, Namespace), { CI = [I] }.
compound_instruction(CI, Namespace) --> instruction(I, Namespace), [tokSColon], compound_instruction(C, Namespace), 
										{ append([I], C, CI) }. %{ CI = instr(I, C) }.

instruction(Instr, Namespace) --> ([tokWhile], !, bool_expr(Bool, Namespace), [tokDo], compound_instruction(Body, Namespace), [tokDone], { Instr = while(Bool, Body) };
						[tokIf], !, bool_expr(Bool, Namespace), [tokThen], compound_instruction(ThenPart, Namespace),
							([tokElse], !, compound_instruction(ElsePart, Namespace), [tokFi], { Instr = if(Bool, ThenPart, ElsePart) };  
							 [tokFi], { Instr = if(Bool, ThenPart) }
		 					);
   						variable(Var, Namespace), !, [tokAssgn], arith_expr(Expr, Namespace), { Instr = assgn(Var, Expr) };
   						[tokCall], !, procedure_call(PC, Namespace), { Instr = call(PC) };
   						[tokReturn], !, arith_expr(AE, Namespace), { Instr = retr(AE) };
   						[tokRead], !, variable(Var, Namespace), { Instr = read(Var) };
   						[tokWrite], !, arith_expr(AE, Namespace), { Instr = write(AE) }
   						).

arith_expr(Expr, Namespace) --> summand(Expr, Namespace).
arith_expr(Expr, Namespace) --> summand(S, Namespace), additive_op(Op), arith_expr(E, Namespace), {Expr =.. [Op, S, E] }.

summand(Expr, Namespace) --> factor(Expr, Namespace).
summand(Expr, Namespace) --> factor(Factor, Namespace), multiplicative_op(Op), !, summand(Summ, Namespace), { Expr =.. [Op, Factor, Summ] }.
%% summand(Acc, Expr) --> summand(Acc1, Expr), multiplicative_op(Op), !, factor(Factor), { Acc1 =.. [Op, Acc, Factor] }.

factor(Expr, Namespace) --> simple_expr(Expr, Namespace), !.
factor(Expr, Namespace) --> [tokMinus], simple_expr(-(Expr), Namespace).
simple_expr(Expr, Namespace) --> atomic_expr(Expr, Namespace), !.% { print("simple expr enter"), nl}.
simple_expr(Expr, Namespace) --> [tokLParen], arith_expr(E, Namespace), [tokRParen], { Expr = (E) }.

atomic_expr(Expr, Namespace) --> procedure_call(Expr, Namespace).
atomic_expr(Expr, Namespace) --> variable(Expr, Namespace).% { print("atomic var enter"), nl, print(Expr), nl}.
atomic_expr(num(Expr), Namespace) --> [tokNumber(Expr)]. %, { Expr = num(N) }.

procedure_call(PC, Namespace) --> 
			[tokName(Name), tokLParen], {print("Proc call enter:"), nl, print(Name), nl}, factual_args(FAs, Namespace), 
			[tokRParen], 
								{ 	
								my_atom_concat(Namespace, Name, ProcName),
								PC = proc_call(ProcName, FAs), print(PC), nl }.

factual_args(FAs, Namespace) --> [], { FAs = [], print("empty fac args enter"), nl}.
factual_args(FAs, Namespace) --> {print("NON empty fac args enter"), nl}, factual_args_series(FAs, Namespace).

%% factual_args_series(FAS, Namespace) --> {print("enter1"), nl}, [], {FAS = []}.
factual_args_series(FAS, Namespace) --> {print("enter2"), nl}, factual_arg(FA, Namespace), { FAS = [FA] }.
factual_args_series(FAS, Namespace) --> {print("enter3"), nl}, factual_arg(FA, Namespace), [tokComma], 
										factual_args_series(FASs, Namespace), { append([FA], FASs, FAS) }.

factual_arg(FA, Namespace) --> {print("FAC ARGS ENTER"), nl}, arith_expr(FA, Namespace),
								{print("factual_arg as arith_expr:"), nl, print(FA), nl}.

bool_expr(B, Namespace) --> conjunct(B, Namespace), !.
bool_expr(B, Namespace) --> conjunct(C, Namespace), [tokOr], bool_expr(Be, Namespace), { B = or(C, Be) }.

conjunct(C, Namespace) --> condition(C, Namespace), !.
conjunct(C, Namespace) --> condition(Con, Namespace), [tokAnd], conjunct(Coj, Namespace), 
							{ C = not(or(not(Con), not(Coj))) }.

condition(C, Namespace) --> rel_expr(C, Namespace), !.
condition(C, Namespace) --> [tokNot], rel_expr(R, Namespace), { C = not(R) }.

rel_expr(R, Namespace) --> arith_expr(AL, Namespace), rel_op(Rop), arith_expr(AR, Namespace), { R =.. [Rop, AL, AR] }.
rel_expr((R), Namespace) --> [tokLParen], bool_expr(R, Namespace), [tokRParen].

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

%% TODO: Matchowanie zmiennych nie lokalnych 

getDeclarations(blck([], _), [], []) :- !.
getDeclarations(blck([var(V) | Rest], _), [V | VarDecls], ProcDecls) :- !,
	getDeclarations(blck(Rest, _), VarDecls, ProcDecls).
getDeclarations(blck([proc(Name, Args, blck(BlckDecls, Code)) | Rest], _), VarDecls, ProcDecls) :-
	getDeclarations(blck(BlckDecls, Code), VarDeclsBlck, ProcDeclsBlck),
	getDeclarations(blck(Rest, _), VarDeclsNext, ProcDeclsNext),
	append(VarDeclsBlck, VarDeclsNext, VarDeclsTemp),
	append([(Name, Args, Code) | ProcDeclsBlck], ProcDeclsNext, ProcDecls),
	getDeclarations(Args, VarDeclsArgs, _),
	append(VarDeclsArgs, VarDeclsTemp, VarDecls).

getDeclarations([], [], []).
getDeclarations([byValueArg(Var) | Rest], [Var | VarDecls], ProcDecls) :-
	getDeclarations(Rest, VarDecls, ProcDecls).
getDeclarations([byNameArg(Var) | Rest], VarDecls, ProcDecls) :-
	getDeclarations(Rest, VarDecls, ProcDecls).

makeProcDeclsDict([], d{}) :- !.
makeProcDeclsDict([(Name, Args, Code) | Rest], Dict) :-
	makeProcDeclsDict(Rest, PrevDict),
	addToDict(PrevDict, Name, (Args, Code), Dict).

assembler(Prog, Code, NonHex) :-
	getDeclarations(Prog, VarDecls, ProcDecls),
	makeProcDeclsDict(ProcDecls, ProcDeclsDict),
	print(ProcDeclsDict), nl,
	encode(Prog, Code, ProcDeclsDict),
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


encode(blck(Declarations, Instructions), Code, ProcDecls) :- 
	encode(Instructions, BlckCode, ProcDecls),
	append(BlckCode, [const, 0, syscall], Code).
	
encode([], [], ProcDecls) :- !.
encode([Instruction | Rest], Code, ProcDecls) :- 
	encode(Instruction, Cmd, ProcDecls),
	encode(Rest, Cmds, ProcDecls),
	append(Cmd, Cmds, Code).

% predykat zapisuje ACC do tymczasowej komórki i ją przeskakuje
storeAccToTemp([swapd, const, currPos(VarPos, 7), swapa, swapd, 
				store, const, currPos(3), jump, nop], currPos(VarPos, 7)). 


%% WYRAŻENIA ATOMOWE %%

% załaduj liczbę do ACC
encode(num(N), [const, N], ProcDecls).

% załaduj adres zmiennej do ACC
% przenieś adres z ACC do AR
% załaduj zmienną (LOAD)
encode(var(V), [const, var(V), swapa, load], ProcDecls).


%% INSTRUKCJE %%

encode(instr(I), Code, ProcDecls) :- encode(I, Code, ProcDecls).

% policz prawą stronę wyrazenia
% wynik musi być w ACC
% przenieś wynik z ACC do DR
% załaduj adres zmiennej do ACC
% przenieś adres z ACC do AR
% przenieś wynik z DR do ACC
% zapisz ACC do komórki nr z AR
encode(assgn(V, Expr), Code, ProcDecls) :- 
	encode(Expr, ExprCmds, ProcDecls),
	Cmds = [swapd, const, V, swapa, swapd, store],
	append(ExprCmds, Cmds, Code).

% załaduj 1 do ACC (kod SYSCALL::READ)
% SYSCALL (read)
% przenieś wczytane słowo z ACC do DR
% załaduj adres zmiennej do ACC
% przenieś adres zmiennej z ACC do AR
% przenieś słowo z DR do ACC
% zapisz słowo (ACC) do komórki nr z AR
encode(read(V), Code, ProcDecls) :- 
	Code = [const, 1, syscall, swapd, const, V, swapa, swapd, store].

% enkoduj wyrazenie
% wynik jest w ACC
% przenieś wynik z ACC do DR
% załaduj 2 do ACC (kod SYSCALL::WRITE)
% SYSCALL (write)
encode(write(Expr), Code, ProcDecls) :- 
	encode(Expr, Cmds, ProcDecls),
	append(Cmds, [swapd, const, 2, syscall], Code).
	%% Code = [const, V, swapa, load, swapd, const, 2, syscall].

% enkoduj instrukcje warunku: w akumulatorze jest -1 jeśli warunek jest prawdziwy 
% lub 0 jeśli jest fałszywy 
% przenieś wynik z ACC do DR
% załaduj adres pierwszej komórki pamięci po zakończeniu pętli: current_pos + liczba_intrukcji_w_pętli + 3 + 3 + 1
% skocz poza pętle jeśli w ACC jest 0 (BRANCHZ)
% enkoduj instrukcje w pętli
% skocz do komórki pamięci current_pos - (liczba_instrukcji_w_pętli + 
% 									  + liczba_instrukcji_w_warunku + 1)
encode(while(Cond, Instrs), Code, ProcDecls) :- 
	encode(Cond, CondCmds, ProcDecls), encode(Instrs, InstrCmds, ProcDecls),
	length(CondCmds, CondCnt), length(InstrCmds, InstrCnt),
	append(CondCmds, [swapd, const, currPos(InstrCnt + 3 + 3 + 1), swapa, swapd, branchz], Cmds),
	
	length(Cmds, CmdsCnt),
	append(InstrCmds, [const, currPos(-InstrCnt - CmdsCnt - 1), jump], EndingCmds),
	append(Cmds, EndingCmds, Code).

encode(if(Bool, ThenPart, ElsePart), Code, ProcDecls) :-  !,
	encode(Bool, BoolTempCmds, ProcDecls),
	encode(ElsePart, ElseCmds, ProcDecls),
	length(ElseCmds, ElseCnt),
	
	encode(ThenPart, ThenTempCmds, ProcDecls),
	append(ThenTempCmds, [const, currPos(2 + ElseCnt), jump], ThenCmds),
	length(ThenCmds, ThenCnt),

	append(BoolTempCmds, [swapd, const, currPos(4 + ThenCnt), swapa, swapd, branchz], BoolCmds),
	append(BoolCmds, [ThenCmds, ElseCmds], TempCode),
	append(TempCode, Code).

encode(if(Bool, ThenPart), Code, ProcDecls) :-
	encode(Bool, BoolTempCmds, ProcDecls),	
	encode(ThenPart, ThenCmds, ProcDecls),
	length(ThenCmds, ThenCnt),
	append(BoolTempCmds, [swapd, const, currPos(4 + ThenCnt), swapa, swapd, branchz], BoolCmds),
	append(BoolCmds, ThenCmds, Code).


%% getFromDict(Key, Dict, Value) :-
%% 	is_dict(Dict), print("IS DICT"), nl, print(Dict), nl,
%% 	Value = Dict.get(Key).

%% getProcAbsynt(_())

replaceLabelPos(_, [], Pos, [], Cnt) :- !.
replaceLabelPos(Name, [labelPos(Name) | Rest], Pos, [currPos(Pos - Cnt) | Result], Cnt) :- !,
	NextCnt is Cnt + 1,
	replaceLabelPos(Name, Rest, Pos, Result, NextCnt).
replaceLabelPos(Name, [Cmd | Rest], Pos, [Cmd | Result], Cnt) :-
	NextCnt is Cnt + 1,
	replaceLabelPos(Name, Rest, Pos, Result, NextCnt).

replace([],[], PrevAtom, NextAtom).
replace([H | T], [NextH | NextT], PrevAtom, NextAtom):-
    (
    	H == PrevAtom 
    	-> NextH = NextAtom;  
    	replace(H, NextH, PrevAtom, NextAtom) 
    ),
	replace(T, NextT, PrevAtom, NextAtom).

replace(L, R, PrevAtom, NextAtom):-
	L =.. [Functor | ArgsL],
	replace(ArgsL, ArgsR, PrevAtom, NextAtom),
	R =.. [Functor | ArgsR].

%% replace(X,Y,X,Y) :- !.
%% replace(X,Y,S,R) :-
%%     S =.. [F|As], maplist(replace(X,Y),As,Rs), R =.. [F|Rs], !.
%% replace(_,_,U,U).

loadProcedureArgs([], [], [], _) :- !.
loadProcedureArgs([Arg | CallArgs], [byValueArg(Var) | ProcArgs], Code, ProcDecls) :-
	encode(Arg, ArgCmds, ProcDecls),
	loadProcedureArgs(CallArgs, ProcArgs, LoadCmds, ProcDecls),
	append(ArgCmds, [swapd, const, Var, swapa, swapd, store], Cmds),
	append(Cmds, LoadCmds, Code).

loadProcedureArgs([Arg | CallArgs], [byNameArg(Var) | ProcArgs], Code, ProcDecls) :-
	loadProcedureArgs(CallArgs, ProcArgs, Code, ProcDecls).

replaceByNameArgs([], [], P, P) :- !.
replaceByNameArgs([Arg | CallArgs], [byNameArg(Var) | ProcArgs], CurrentProc, ResultProc) :-
	replace(CurrentProc, Proc, var(Var), Arg), !,
	replaceByNameArgs(CallArgs, ProcArgs, Proc, ResultProc).

replaceByNameArgs([Arg | CallArgs], [byValueArg(Var) | ProcArgs], CurrentProc, ResultProc) :-
	replaceByNameArgs(CallArgs, ProcArgs, CurrentProc, ResultProc).

encode(proc_call(Name, CallArgs), Code, ProcDecls) :-
	%% print("name: "), nl, print(Name), nl,
	%% getFromDict(Name, ProcDecls, Val).
	(ProcArgs, Proc) = ProcDecls.get(Name),
	loadProcedureArgs(CallArgs, ProcArgs, LoadCmds, ProcDecls),
	replaceByNameArgs(CallArgs, ProcArgs, Proc, ResultProc),
	
	print("proc:"), nl, print(Proc), nl,
	print("ResultProc: "), nl, print(ResultProc), nl,

	encode(ResultProc, ProcCmds, ProcDecls),
	(
		nth0(Npos, ProcCmds, labelPos(jumpProcEnd)),
		length(ProcCmds, ProcCmdsLen), 
		replaceLabelPos(jumpProcEnd, ProcCmds, ProcCmdsLen, ProcCmdsReplaced, 0),
		append(ProcCmdsReplaced, [swapd], Code);
		append(ProcCmds, [const, 0], Code)
	).

	%% append(ProcCmds, [swapd], Code).
	%% atom_concat(Name, 'end', LabelName),
	%% print("proc call:"), nl, print(Name), nl, print(Args), nl.


encode(retr(Expr), Code, ProcDecls) :-
	encode(Expr, ExprCode, ProcDecls),
	append(ExprCode, [swapd, const, labelPos(jumpProcEnd), jump], Code).
%% OPERATORY RELACYJNE %%

% Oblicz L - R w ACC
% jeśli wynik jest równy 0 to go pozostaw w ACC ( 0 == false)
% i przeskocz ustawianie ACC na -1 ( -1 == true)
encode(<>(L, R), Code, ProcDecls) :- 
	arithm_encode(L, R, sub, Cmds, ProcDecls),
	negativeNumber(1, MinusOne),
	append(Cmds, [swapd, const, currPos(6), swapa, swapd, branchz, const, MinusOne], Code).

% Oblicz L <> R, zaneguj ACC
encode(=(L, R), Code, ProcDecls) :-
	encode(<>(L, R), Cmds, ProcDecls),
	negateACC(Neg),
	append(Cmds, Neg, Code).
	%% arithm_encode(L, R, sub, Cmds),
	%% negativeNumber(1, MinusOne),
	%% append(Cmds, [swapd, const, currPos(10), swapa, swapd, branchz, const, 0, swapd, const, currPos(5), jump,
	%% 					const, MinusOne, swapd, swapd], Code).

% Oblicz L - R,
% Jeśli ACC < 0 to przeskocz ustawianie ACC na 0 (ACC < 0 to true)
encode(<(L, R), Code, ProcDecls) :-
	arithm_encode(L, R, sub, Cmds, ProcDecls),
	negativeNumber(1, MinusOne),
	append(Cmds, [swapd, const, currPos(6), swapa, swapd, branchn, const, 0], Code).

encode(>(L, R), Code, ProcDecls) :- encode(<(R, L), Code, ProcDecls).
encode(<=(L, R), Code, ProcDecls) :-
	encode(>(L, R), Cmds, ProcDecls),
	negateACC(Neg),
	append(Cmds, Neg, Code).

encode(>=(L, R), Code, ProcDecls) :- encode(<=(R, L), Code, ProcDecls).


%% OPERATORY ARYTMETYCZNE %%

% enkoduj lewą stronę, wynik przenieś z ACC do currPos(VarPos)
% przeskocz currPos(VarPos)
% enkoduj prawą stronę, wynik przenieś z ACC do DR
% załaduj currPos(VarPos) do ACC
% wykonaj operacje arytm DR do ACC
arithm_encode(L, R, Cmd, Code, ProcDecls) :-
	encode(L, LCmdsTemp, ProcDecls),
	storeAccToTemp(StoreCmds, VarPos),
	append(LCmdsTemp, StoreCmds, LCmds),
	encode(R, RCmdsTemp, ProcDecls),
	append(RCmdsTemp, [swapd, const, VarPos, swapa, load, Cmd], RCmds),
	append(LCmds, RCmds, Code).

%% TODO: Operator modulo (mod(L, R))
encode(L + R, Code, ProcDecls) :- arithm_encode(L, R, add, Code, ProcDecls).
encode(L - R, Code, ProcDecls) :- arithm_encode(L, R, sub, Code, ProcDecls).
encode(div(L, R), Code, ProcDecls) :- arithm_encode(L, R, div, Code, ProcDecls).
encode(L * R, Code, ProcDecls) :- arithm_encode(L, R, mul, Code, ProcDecls).


%% OPERATORY LOGICZNE %%

% enkoduj wyrazenie, wynik jest w ACC
% zaneguj ACC
encode(not(Expr), Code, ProcDecls) :-
	encode(Expr, Cmds, ProcDecls),
	negateACC(Neg),
	append(Cmds, Neg, Code).

encode(or(L, R), Code, ProcDecls) :- encode(<((L + R), num(0)), Code, ProcDecls).
% nie będzie 'and' do enkodowania bo: L and R jest zamieniane w parserze na not(not(L) or not(R))

