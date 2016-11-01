outputFile('./slither_solved.txt').
inputFile('./slither_unsolved.txt').

/********************* dummy solution algorithms -> fill your correct algorithm here */
/* doSolve(SizeX,SizeY,Input,Output) */
doSolve(_,_,Solution,Solution).

/********************* writing the result */
writeFullOutput(S, X, Y):- write(X), write('x'), write(Y), nl, writeOutput(S,X).

writeOutput([],X):- writeUnknownHorizontal(X).
writeOutput([E|R],X):- writeUnknownHorizontal(X), writeLine(E), writeOutput(R,X).

writeUnknownHorizontal(0):- write('+'), nl.
writeUnknownHorizontal(N):- N>0, N1 is N-1, write('+'), write('-'), writeUnknownHorizontal(N1).

writeLine([]):- write('|'), nl.
writeLine([E|R]):- write('|'), write(E), writeLine(R).

/********************** reading the input */
readProblem(N,M,Problem):- readInt(N), readInt(M), length(Problem, M), readProblemLines(N,Problem).

readProblemLines(_,[]).
readProblemLines(N,[H|T]):- length(H,N), readLine(H), readProblemLines(N,T).

readLine([]).
readLine([E|R]):- readCount(E), readLine(R).

readCount(E):- repeat, get_code(M), translate(M,E), !.

translate(42,'?').
translate(48,0).
translate(49,1).
translate(50,2).
translate(51,3).
translate(52,4).
translate(-1,'@'). /*EOF*/

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.

/*********************** global control: starting the algorithm and the reading */
run:- inputFile(IF), see(IF), outputFile(F), tell(F), readInt(N), write(N), nl, solveProblems(N), told, seen, !.
run:- told, seen. /* close the files */

solveProblems(0).
solveProblems(N):- N>0, readProblem(X, Y, I), doSolve(X, Y, I, S), writeFullOutput(S, X, Y), !, N1 is N-1, solveProblems(N1).

:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

