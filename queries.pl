string_codes("local tr, tr, aa local bb procedure func ( y, x, trttr ) begin y := 2; end", S), phrase(lexer(TokList), S), phrase(declarations(X), TokList).

phrase(declarations(X), [tokLocal, tokName(tr), tokComma, tokName(tr), tokComma, tokName(aa), tokLocal, tokName(bb), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokComma, tokName(trttr), tokRParen, tokName(beg), tokName(y), tokAssgn, tokNumber(2), tokSColon, tokEnd]). 


% works 
phrase(declarations(X), [tokLocal, tokName(tr), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokRParen, tokLocal, tokName(varT), tokBeg, tokRead, tokName(y), tokSColon, tokRead, tokName(x), tokEnd]). 

% doesn't / now works
phrase(declarations(X), [tokLocal, tokName(tr), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokRParen, tokLocal, tokName(varT), tokBeg, tokName(y), tokAssgn, tokName(3), tokSColon, tokRead, tokName(x), tokEnd]). 

string_codes("program Prog local tr, tr, aa local bb procedure func ( y, x, trttr ) begin y := 2 end begin tr := 3 end", S), parse(S, X).

string_codes("program Suma local x, s begin s := 0; read x; while x <> 0 do s:= s + x; read x; done; write s end", S), parse(S, X).

string_codes("program Suma local x, s begin s := 0; read x; while x <> 0 do s:= s + x; read x; done; write s end", S), phrase(lexer(TokList), S).


phrase(declarations(X), [tokLocal, tokName(tr), tokProcedure, tokName(func), tokLParen, tokName(y), tokComma, tokName(x), tokRParen, tokLocal, tokName(varT), tokBeg, tokName(y), tokAssgn, tokNumber(2), tokEnd]). 

phrase(program(X), [tokProgram, tokName('Suma'), tokLocal, tokName(x), tokComma, tokName(s), tokBeg, tokName(s), tokAssgn, tokNumber(0), tokSColon, tokRead, tokName(x), tokSColon, tokWhile, tokName(x), tokNeq, tokNumber(0), tokDo, tokName(s), tokAssgn, tokName(s), tokPlus, tokName(x), tokSColon, tokRead, tokName(x), tokSColon, tokDone, tokSColon, tokWrite, tokName(s), tokEnd])
