% Autor: Mateusz Lewko
% Rozwiązanie za 47 punktów: wszystko bez rekurencji 
% 
% Ciało funkcji jest wklejane w miejsce wywołania.
% Wszystkie zmienne są statyczne (wliczając argumenty funkcji)
% 
% predykat do wczytywania z pliku: compileFile, (plik musi zawierać kod w następujacej postaci: "<kod programu>". 
% czyli w cudzysłowie i z kropką na końcu)
% Błędy: 
%		1) czasami nie działają komentarze, tzn jeśli nie ma spacji przed i po (* oraz *)
%			jezeli predykat zwaraca false nawet po sformatowaniu komentarzy, to proszę spróbować usunąć komentarze


algol16(Source, SextiumBin) :-
	phrase(lexer(TokList), Source),
	phrase(program(Absynt), TokList),
	assembler(Absynt, Code, SextiumBin).


%% LEXER %% 

%% Na bazie while_parser.pl ze strony KNO
lexer(Tokens) -->
   white_space,
   comment,
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

without_comment_end --> [CharA, CharB], { \+ CharA == 42, \+ CharB == 41}, without_comment_end.
without_comment_end --> [].
comment --> [40, 42], !, without_comment_end, [42, 41].
comment --> [].

white_space --> [Char], { code_type(Char, space) }, !, white_space.
white_space --> [].

digit(D) --> [D], { code_type(D, digit) }.
digits([D|T]) --> digit(D), !, digits(T).
digits([]) --> [].

number(D, N) --> digits(Ds), { number_chars(N, [D | Ds]) }.

letter(L) --> [L], { code_type(L, alpha) }.

alphanum([A|T]) --> [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) --> [].

csymID([A|T]) --> [A], { code_type(A, csym) }, !, csymID(T).
csymID([]) --> [].

identifier(L, Id) --> csymID(As), { atom_codes(Id, [L | As]) }.


%% PARSER %% 

% predykat pomocniczy, łączy dwa atomy w jeden, oddzielając je znakiem '|', 
% słuzy do tworzenia przestrzeni nazw zmiennych i procedur
my_atom_concat(Prev, Name, Next) :- atomic_list_concat([Prev, Name], '|', Next).

program(P) --> [tokProgram, tokName(Name)], block(P, global).

block(B, Namespace) --> declarations(D, Namespace), [tokBeg], compound_instruction(CI, Namespace), [tokEnd], { B = blck(D, CI) }.

declarations(D, Namespace) --> [], { D = [] }.
declarations(D, Namespace) --> declaration(Dec, Namespace), declarations(Decs, Namespace), { append(Dec, Decs, D) }.
declaration(D, Namespace) --> declarator(D, Namespace).
declaration(D, Namespace) --> procedure(P, Namespace), { D = [P]}.
declarator(D, Namespace) --> [tokLocal], variables(D, Namespace).

variables(V, Namespace) --> variable(Var, Namespace), { V = [Var]}.
variables(V, Namespace) --> variable(Var, Namespace), [tokComma], variables(Vars, Namespace), 
							{ append([Var], Vars, V) }.
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

compound_instruction(CI, Namespace) --> instruction(I, Namespace), { CI = [I] }.
compound_instruction(CI, Namespace) --> instruction(I, Namespace), [tokSColon], compound_instruction(C, Namespace), 
										{ append([I], C, CI) }. 

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

arith_expr(Expr, Namespace) --> summand(Summand, Namespace), arith_expr(Summand, Expr, Namespace).
arith_expr(Acc, Expr, Namespace) --> additive_op(Op), summand(Summand, Namespace),
      								{ Acc1 =.. [Op, Acc, Summand] }, arith_expr(Acc1, Expr, Namespace).

arith_expr(Acc, Acc, Namespace) --> [].

summand(Expr, Namespace) -->
   factor(Factor, Namespace), summand(Factor, Expr, Namespace).

summand(Acc, Expr, Namespace) -->
   	multiplicative_op(Op), factor(Factor, Namespace),
    { Acc1 =.. [Op, Acc, Factor] }, summand(Acc1, Expr, Namespace).
summand(Acc, Acc, Namespace) --> [].


factor(-(Expr), Namespace) --> [tokMinus], simple_expr(Expr, Namespace).
factor(Expr, Namespace) --> simple_expr(Expr, Namespace).

simple_expr(Expr, Namespace) --> atomic_expr(Expr, Namespace), !.
simple_expr(Expr, Namespace) --> [tokLParen], arith_expr(E, Namespace), [tokRParen], { Expr = (E) }.

atomic_expr(Expr, Namespace) --> procedure_call(Expr, Namespace).
atomic_expr(Expr, Namespace) --> variable(Expr, Namespace).
atomic_expr(num(Expr), Namespace) --> [tokNumber(Expr)].

procedure_call(PC, Namespace) --> 
			[tokName(Name), tokLParen], factual_args(FAs, Namespace), [tokRParen], 
			{ my_atom_concat(Namespace, Name, ProcName), PC = proc_call(ProcName, FAs) }.

factual_args(FAs, Namespace) --> [], { FAs = [] }.
factual_args(FAs, Namespace) --> factual_args_series(FAs, Namespace).

factual_args_series(FAS, Namespace) --> factual_arg(FA, Namespace), { FAS = [FA] }.
factual_args_series(FAS, Namespace) --> factual_arg(FA, Namespace), [tokComma], 
										factual_args_series(FASs, Namespace), 
										{ append([FA], FASs, FAS) }.

factual_arg(FA, Namespace) --> arith_expr(FA, Namespace).

bool_expr(Bool, Namespace) --> disjunct(Disjunct, Namespace), bool_expr(Disjunct, Bool, Namespace).

bool_expr(Acc, Bool, Namespace) -->
   [tokOr], !, disjunct(Disjunct, Namespace),
   { Acc1 =.. [or, Acc, Disjunct] }, bool_expr(Acc1, Bool, Namespace).
bool_expr(Acc, Acc, Namespace) --> [].

disjunct(Disjunct, Namespace) -->
   conjunct(Conjunct, Namespace), disjunct(Conjunct, Disjunct, Namespace).

disjunct(Acc, Disjunct, Namespace) -->
   [tokAnd], !, conjunct(Conjunct, Namespace),
      { Acc1 = not(or(not(Acc), not(Conjunct))) }, disjunct(Acc1, Disjunct, Namespace).
disjunct(Acc, Acc, Namespace) --> [].

conjunct(Conjunct, Namespace) -->
   (  [tokLParen], !, bool_expr(Conjunct, Namespace), [tokRParen]
   ;  [tokNot], !, conjunct(NotConjunct, Namespace),
         { Conjunct = not(NotConjunct) }
   ;  arith_expr(LExpr, Namespace), rel_op(Op), arith_expr(RExpr, Namespace),
         { Conjunct =.. [Op, LExpr, RExpr] }
   ).

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


%% ASSEMBLER %%

% konwertowanie etykiet z assemblera na liczby
lookupCmdCode(Cmd, Code) :- 
	Code = d{nop:0, syscall:1, load:2, store:3, swapa:4, swapd:5,
			branchz:6, branchn:7, jump:8, const:9, add:10, sub:11, mul:12, div:13}.get(Cmd).

% Zamiana currPos(Pos, Delta) na pozycje Pos + Delta
% currPos(...) to miejsca zmiennych / skoków w pamięci
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

addToDict(Dict, Key, Value, Dict.put(Key, Value)).

% Zamiana zmiennych na adresy ze słownika (powstałego z deklaracji zmiennych)
setVarAdresses([], [], D).
setVarAdresses([var(V) | Rest], [Var | Result], VarsDict) :- !,
	getMatchedFromDict(V, VarsDict, Var),
	setVarAdresses(Rest, Result, VarsDict).

setVarAdresses([Cmd | Rest], [Cmd | Result], VarsDict) :- 
	setVarAdresses(Rest, Result, VarsDict).

% zamiana currPos(N) na <pozycja currPos(N) na liście komend> + N
% zamiana etykiet na liczby 
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

% wyciąganie deklaracji z bloków i argumentów procedur (wszystkie będą traktowane jak zmienne statyczne)
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

% słownik powstały z deklaracji procedur
makeProcDeclsDict([], d{}) :- !.
makeProcDeclsDict([(Name, Args, Code) | Rest], Dict) :-
	makeProcDeclsDict(Rest, PrevDict),
	addToDict(PrevDict, Name, (Args, Code), Dict).

% słownik powstały z deklaracji zmiennych
makeVarDeclsDict([], d{}, _) :- !.
makeVarDeclsDict([Var | Rest], DictRes, Cnt) :-
	Val is 2^16 - Cnt - 1,
	NextCnt is Cnt + 1,
	makeVarDeclsDict(Rest, Dict, NextCnt),
	DictRes = Dict.put(Var, Val).

% wyciąganie deklaracji
% enkodowanie AST
% obliczanie pozycji / numerów w pamięci
% zamiana na assembler
assembler(Prog, Code, NonHex) :-
	getDeclarations(Prog, VarDecls, ProcDecls),
	makeProcDeclsDict(ProcDecls, ProcDeclsDict),
	
	encode(Prog, Code, ProcDeclsDict),
	
	setJumpPos(Code, 0),
	makeVarDeclsDict(VarDecls, VarDeclsDict, 0),
	setVarAdresses(Code, VarsWithAdresses, VarDeclsDict),
	setSymbols(VarsWithAdresses, NonHex, 0).


%% ENKODER %%

%% POMOCNICZE %%

% wczytuje plik, zwaraca assembler, AST, assembler z etykietami
compileFile(FileName, NonHex, Code, Absynt) :-
	open(FileName, 'read', Rstream), 
	read(Rstream, SourceCode), 
	close(Rstream),
	string_codes(SourceCode, Codes),
	phrase(lexer(TokList), Codes),
	phrase(program(Absynt), TokList),
	assembler(Absynt, Code, NonHex).

% Zwraca ujemną liczbe w Alogolu 16
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


replaceLabelPos(_, [], Pos, [], Cnt) :- !.
replaceLabelPos(Name, [labelPos(Name) | Rest], Pos, [currPos(Pos - Cnt) | Result], Cnt) :- !,
	NextCnt is Cnt + 1,
	replaceLabelPos(Name, Rest, Pos, Result, NextCnt).
replaceLabelPos(Name, [Cmd | Rest], Pos, [Cmd | Result], Cnt) :-
	NextCnt is Cnt + 1,
	replaceLabelPos(Name, Rest, Pos, Result, NextCnt).

% zamiana atomów PrevAtom na NextAtom w AST dla danej procedury
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

% enkodowanie argumentów przy wywoływaniu procedury
% zapisywanie wyniku kazedego arguemntu do komórki pamięci (ArgsAdresses)
encodeProcedureArgs([], [], [], [], _) :- !.
encodeProcedureArgs([Arg | CallArgs], [byValueArg(Var) | ProcArgs], Code, [ArgPos | ArgsAdresses], ProcDecls) :-
	encode(Arg, ArgCmds, ProcDecls),
	storeAccToTemp(StoreCmds, ArgPos),
	encodeProcedureArgs(CallArgs, ProcArgs, LoadCmds, ArgsAdresses, ProcDecls),
	append(ArgCmds, StoreCmds, Cmds),
	append(LoadCmds, Cmds, Code).

encodeProcedureArgs([Arg | CallArgs], [byNameArg(Var) | ProcArgs], Code, [doNothing | ArgsAdresses], ProcDecls) :-
	encodeProcedureArgs(CallArgs, ProcArgs, Code, ArgsAdresses, ProcDecls).


loadProcedureArgs([], [], [], []) :- !.
loadProcedureArgs([Arg | CallArgs], [byValueArg(Var) | ProcArgs], Code, [ArgPos | ArgsAdresses]) :-
	loadProcedureArgs(CallArgs, ProcArgs, Cmds, ArgsAdresses),
	append([const, ArgPos, swapa, load, swapd, const, var(Var), swapa, swapd, store], Cmds, Code).

loadProcedureArgs([Arg | CallArgs], [byNameArg(Var) | ProcArgs], Code, [doNothing | ArgsAdresses]) :-
	loadProcedureArgs(CallArgs, ProcArgs, Code, ArgsAdresses).

% zamiana wszystkich argumentów byName w AST procedury
replaceByNameArgs([], [], P, P) :- !.
replaceByNameArgs([Arg | CallArgs], [byNameArg(Var) | ProcArgs], CurrentProc, ResultProc) :-
	replace(CurrentProc, Proc, var(Var), Arg), !,
	replaceByNameArgs(CallArgs, ProcArgs, Proc, ResultProc).

replaceByNameArgs([Arg | CallArgs], [byValueArg(Var) | ProcArgs], CurrentProc, ResultProc) :-
	replaceByNameArgs(CallArgs, ProcArgs, CurrentProc, ResultProc).

without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).

% predykat znajduje w słowniku klucz lub klucz który jest 
% najdłuszym prefiksem tego klucza i konkatenacją ostatniego członu
% czyli jak w słowniku są klucze {a|b|c, a|c} i szukamy klucza a|b|d|c to zwróci nam wartość pod kluczem a|b|c
getMatchedFromDict(Key, Dict, Value) :-
	Value = Dict.get(Key), !.

getMatchedFromDict(Key, Dict, Value) :-
	atomic_list_concat(KeyPath, '|', Key),
	last(KeyPath, TrueName),
	select(TrueName, KeyPath, ShorterKeyPath),

	without_last(ShorterKeyPath, NextKeyPath),
	atomic_list_concat(NextKeyPath, '|', ShorterKeyAtom),
	atomic_list_concat([ShorterKeyAtom, TrueName], '|', NextKey),

	getMatchedFromDict(NextKey, Dict, Value).

encode(proc_call(Name, CallArgs), Code, ProcDecls) :-
	getMatchedFromDict(Name, ProcDecls, (ProcArgs, Proc)),
	encodeProcedureArgs(CallArgs, ProcArgs, EncodeCmdsTemp, ArgsAdresses, ProcDecls),
	loadProcedureArgs(CallArgs, ProcArgs, LoadCmdsTemp, ArgsAdresses),
	append(EncodeCmdsTemp, LoadCmdsTemp, LoadCmds),

	replaceByNameArgs(CallArgs, ProcArgs, Proc, ResultProc),

	encode(ResultProc, ProcCmds, ProcDecls),
	(
		nth0(Npos, ProcCmds, labelPos(jumpProcEnd)),
		length(ProcCmds, ProcCmdsLen), 
		replaceLabelPos(jumpProcEnd, ProcCmds, ProcCmdsLen, ProcCmdsReplaced, 0),
		append(ProcCmdsReplaced, [swapd], Cmds),
		append(LoadCmds, Cmds, Code);
		append(ProcCmds, [const, 0], Cmds), 
		append(LoadCmds, Cmds, Code)
	).

encode(retr(Expr), Code, ProcDecls) :-
	encode(Expr, ExprCode, ProcDecls),
	append(ExprCode, [swapd, const, labelPos(jumpProcEnd), jump], Code).

encode(call(Expr), Code, ProcDecls) :- encode(Expr, Code, ProcDecls).

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

encode(-(R), Code, ProcDecls) :- arithm_encode(num(0), R, sub, Code, ProcDecls).
encode(L + R, Code, ProcDecls) :- arithm_encode(L, R, add, Code, ProcDecls).
encode(L - R, Code, ProcDecls) :- arithm_encode(L, R, sub, Code, ProcDecls).
encode(div(L, R), Code, ProcDecls) :- arithm_encode(L, R, div, Code, ProcDecls).
encode(L * R, Code, ProcDecls) :- arithm_encode(L, R, mul, Code, ProcDecls).
encode(mod(L, R), Code, ProcDecls) :- encode(L - (div(L, R) * R), Code, ProcDecls).

%% OPERATORY LOGICZNE %%

% enkoduj wyrazenie, wynik jest w ACC
% zaneguj ACC
encode(not(Expr), Code, ProcDecls) :-
	encode(Expr, Cmds, ProcDecls),
	negateACC(Neg),
	append(Cmds, Neg, Code).

encode(or(L, R), Code, ProcDecls) :- encode(<((L + R), num(0)), Code, ProcDecls).
% nie będzie 'and' do enkodowania bo: L and R jest zamieniane w parserze na not(not(L) or not(R))
