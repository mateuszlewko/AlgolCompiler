string_codes("local tr, tr, aa local bb procedure func ( y, x, trttr ) begin y := 2; end", S), phrase(lexer(TokList), S), phrase(declarations(X), TokList).

phrase(declarations(X), [tokLocal, tokName(tr), tokComma, tokName(tr), tokComma, tokName(aa), tokLocal, tokName(bb), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokComma, tokName(trttr), tokRParen, tokName(beg), tokName(y), tokAssgn, tokNumber(2), tokSColon, tokEnd]). 


% works 
phrase(declarations(X), [tokLocal, tokName(tr), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokRParen, tokLocal, tokName(varT), tokBeg, tokRead, tokName(y), tokSColon, tokRead, tokName(x), tokEnd]). 

% doesn't / now works
phrase(declarations(X), [tokLocal, tokName(tr), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokRParen, tokLocal, tokName(varT), tokBeg, tokName(y), tokAssgn, tokName(3), tokSColon, tokRead, tokName(x), tokEnd]). 

string_codes("program Prog local tr, tr, aa local bb procedure func ( y, x, trttr ) begin y := 2; while y == 0 do y := 0; done; end begin tr := 3 end", S), parse(S, X).

string_codes("program Suma local x, s begin s := 0; read x; while x <> 0 do s:= s + x; read x done; write s end", S), parse(S, X).

string_codes("program Suma local x, s begin s := 0; read x; while x <> 0 do s:= s + x; read x; done; write s end", S), phrase(lexer(TokList), S).

% "program test local s procedure one () begin write 5; return 1 end begin read s; if s + one() > 2 then write one() fi write one() - 1; end"

%% string_codes("program test local s procedure one () begin write 5; return 1 end begin read s; if s + one() > 2 then write one(); fi write one() - 1 end", S), phrase(lexer(TokList), S).
% [tokProgram, tokName(test), tokLocal, tokName(s), tokProcedure, tokName(one), tokLParen, tokRParen, tokBeg, tokWrite, tokNumber(5), tokSColon, tokReturn, tokNumber(1), tokEnd, tokBeg, tokRead, tokName(s), tokSColon, tokIf, tokName(s), tokPlus, tokName(one), tokLParen, tokRParen, tokGt, tokNumber(2), tokThen, tokWrite, tokName(one), tokLParen, tokRParen, tokFi, tokWrite, tokName(one), tokLParen, tokRParen, tokMinus, tokNumber(1), tokEnd]
%% string_codes("program test local s procedure one () begin write 5; return 1 end begin read s end", S), phrase(lexer(TokList), S).

%% phrase(program(X), [tokProgram, tokName(test), tokLocal, tokName(s), tokProcedure, tokName(one), tokLParen, tokRParen, tokBeg, tokWrite, tokNumber(5), tokSColon, tokReturn, tokNumber(1), tokEnd, tokBeg, tokRead, tokName(s), tokSColon, tokIf, tokName(s), tokPlus, tokName(one), tokLParen, tokRParen, tokGt, tokNumber(2), tokThen, tokWrite, tokName(one), tokLParen, tokRParen, tokSColon, tokFi, tokWrite, tokName(one), tokLParen, tokRParen, tokMinus, tokNumber(1), tokEnd]).
% result:
string_codes("program test local s procedure one () begin write 5; return 1 end begin read s; if s + one() > 2 then write one() fi; write one() - 1 end", S), phrase(lexer(TokList), S).
[tokProgram, tokName(test), tokLocal, tokName(s), tokProcedure, tokName(one), tokLParen, tokRParen, tokBeg, tokWrite, tokNumber(5), tokSColon, tokReturn, tokNumber(1), tokEnd, tokBeg, tokRead, tokName(s), tokSColon, tokIf, tokName(s), tokPlus, tokName(one), tokLParen, tokRParen, tokGt, tokNumber(2), tokThen, tokWrite, tokName(one), tokLParen, tokRParen, tokFi, tokSColon, tokWrite, tokName(one), tokLParen, tokRParen, tokMinus, tokNumber(1), tokEnd].

phrase(program(P), [tokProgram, tokName(test), tokLocal, tokName(s), tokProcedure, tokName(one), tokLParen, tokRParen, tokBeg, tokWrite, tokNumber(5), tokSColon, tokReturn, tokNumber(1), tokEnd, tokBeg, tokRead, tokName(s), tokSColon, tokIf, tokName(s), tokPlus, tokName(one), tokLParen, tokRParen, tokGt, tokNumber(2), tokThen, tokWrite, tokName(one), tokLParen, tokRParen, tokFi, tokSColon, tokWrite, tokName(one), tokLParen, tokRParen, tokMinus, tokNumber(1), tokEnd]).

phrase(declarations(X), [tokLocal, tokName(tr), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokRParen, tokLocal, tokName(varT), tokBeg, tokName(y), tokAssgn, tokNumber(2), tokEnd]). 

phrase(program(X), [tokProgram, tokName('Suma'), tokLocal, tokName(x), tokComma, tokName(s), tokBeg, tokName(s), tokAssgn, tokNumber(0), tokSColon, tokRead, tokName(x), tokSColon, tokWhile, tokName(x), tokNeq, tokNumber(0), tokDo, tokName(s), tokAssgn, tokName(s), tokPlus, tokName(x), tokSColon, tokRead, tokName(x), tokSColon, tokDone, tokSColon, tokWrite, tokName(s), tokEnd]).

string_codes("program test local s procedure one () local locvar begin write 5; return 1 end begin read s; if s + one() > 2 then write one() fi; write one() - 1 end", S), phrase(lexer(TokList), S).

string_codes("program test local s procedure one () local locvar procedure two (arg1) begin return 1 end begin write 5; return 1 end begin read s; if s + one() > 2 then write one() fi; write one() - 1 end", S), phrase(lexer(TokList), S).

%while x <> 0 do s:= s + x; read x done;
string_codes("program Suma local x, s begin s := 0; read x; while x <> 0 do x := 5; read x done; write s end", S), parse(S, X).


% Encoder:
encode(blck(decls([var(x),var(s)]),[assgn(var(s),num(0)),read(var(x)),while(<>(var(x),num(0)),[assgn(var(x),num(5)),read(var(x))]),write(var(s))]), C).

% result:
[
const, 0, swapd, const, s, swapa, swapd, store, const, 
1, syscall, swapd, const, x, swapa, swapd, store, const, 
0, swapd, const, x, swapa, load, sub, swapd, const, currPos(6), swapa, swapd, branchz, const, 
-1, swapd, const, currPos(17+3+3+1), swapa, swapd, branchz, const, 
5, swapd, const, x, swapa, swapd, store, const, 
1, syscall, swapd, const, x, swapa, swapd, store, const, 
currPos(- 17-22-1), jump, const, var(s), swapa, load, swapd, const, 2, syscall].

% result v2:
C = [const, 0, swapd, const, var(s), swapa, swapd, store, const, 1, syscall, swapd, const, var(x), swapa, swapd, store, const, 0, swapd, const, currPos(21, 7), swapa, swapd, store, const, currPos(2), jump, nop, const, var(x), swapa, load, swapd, const, currPos(21, 7), swapa, load, sub, swapd, const, currPos(6), swapa, swapd, branchz, const, 65535, swapd, const, currPos(17+3+3+1), swapa, swapd, branchz, const, 5, swapd, const, var(x), swapa, swapd, store, const, 1, syscall, swapd, const, var(x), swapa, swapd, store, const, currPos(- 17-36-1), jump, const, var(s), swapa, load, swapd, const, 2, syscall],
H = [9, 0, 5, 9, 0, 4, 5, 3, 9, 1, 1, 5, 9, 1, 4, 5, 3, 9, 0, 5, 9, 28, 4, 5, 3, 9, 28, 8, 0, 9, 1, 4, 2, 5, 9, 28, 4, 2, b, 5, 9, 47, 4, 5, 6, 9, 65535, 5, 9, 73, 4, 5, 6, 9, 5, 5, 9, 1, 4, 5, 3, 9, 1, 1, 5, 9, 1, 4, 5, 3, 9, 17, 8, 9, 0, 4, 2, 5, 9, 2, 1].

%%%%%%

stringCompile("program Suma local x, s begin s := 0; read x; while x <> 0 do s:= s + x; read x; done; write s end", A).

% result:
Code = [const, 0, swapd, const, var(s), swapa, swapd, store, const, 1, syscall, swapd, const, var(x), swapa, swapd, store, const, var(x), swapa, load, swapd, const, currPos(23, 7), swapa, swapd, store, const, currPos(3), jump, nop, const, 0, swapd, const, currPos(23, 7), swapa, load, sub, swapd, const, currPos(6), swapa, swapd, branchz, const, 65537, swapd, const, currPos(39+3+3+1), swapa, swapd, branchz, const, var(s), swapa, load, swapd, const, currPos(59, 7), swapa, swapd, store, const, currPos(3), jump, nop, const, var(x), swapa, load, swapd, const, currPos(59, 7), swapa, load, add, swapd, const, var(s), swapa, swapd, store, const, 1, syscall, swapd, const, var(x), swapa, swapd, store, const, currPos(- 39-36-1), jump, const, var(s), swapa, load, swapd, const, 2, syscall, const, 0, syscall],
A = [9, 0, 5, 9, 0, 4, 5, 3, 9, 1, 1, 5, 9, 1, 4, 5, 3, 9, 1, 4, 2, 5, 9, 30, 4, 5, 3, 9, 31, 8, 0, 9, 0, 5, 9, 30, 4, 2, 11, 5, 9, 47, 4, 5, 6, 9, 65537, 5, 9, 95, 4, 5, 6, 9, 0, 4, 2, 5, 9, 66, 4, 5, 3, 9, 67, 8, 0, 9, 1, 4, 2, 5, 9, 66, 4, 2, 10, 5, 9, 0, 4, 5, 3, 9, 1, 1, 5, 9, 1, 4, 5, 3, 9, 17, 8, 9, 0, 4, 2, 5, 9, 2, 1, 9, 0, 1] .

% result v2:
Code = [const, 0, swapd, const, var(s), swapa, swapd, store, const, 1, syscall, swapd, const, var(x), swapa, swapd, store, const, var(x), swapa, load, swapd, const, currPos(23, 7), swapa, swapd, store, const, currPos(3), jump, nop, const, 0, swapd, const, currPos(23, 7), swapa, load, sub, swapd, const, currPos(6), swapa, swapd, branchz, const, 65537, swapd, const, currPos(39+3+3+1), swapa, swapd, branchz, const, var(s), swapa, load, swapd, const, currPos(59, 7), swapa, swapd, store, const, currPos(3), jump, nop, const, var(x), swapa, load, swapd, const, currPos(59, 7), swapa, load, add, swapd, const, var(s), swapa, swapd, store, const, 1, syscall, swapd, const, var(x), swapa, swapd, store, const, currPos(- 39-36-1), jump, const, var(s), swapa, load, swapd, const, 2, syscall, const, 0, syscall],
A = [9, 0, 5, 9, 20000, 4, 5, 3, 9, 1, 1, 5, 9, 20001, 4, 5, 3, 9, 20001, 4, 2, 5, 9, 30, 4, 5, 3, 9, 31, 8, 0, 9, 0, 5, 9, 30, 4, 2, 11, 5, 9, 47, 4, 5, 6, 9, 65537, 5, 9, 95, 4, 5, 6, 9, 20000, 4, 2, 5, 9, 66, 4, 5, 3, 9, 67, 8, 0, 9, 20001, 4, 2, 5, 9, 66, 4, 2, 10, 5, 9, 20000, 4, 5, 3, 9, 1, 1, 5, 9, 20001, 4, 5, 3, 9, 17, 8, 9, 20000, 4, 2, 5, 9, 2, 1, 9, 0, 1] 
