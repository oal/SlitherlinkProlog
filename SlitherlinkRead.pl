outputFile('./slither_solved.txt').
inputFile('./slither_unsolved.txt').

side(0).
side(1).

getNum(X, Y, SizeX, SizeY, Input, N):-
    X < SizeX, Y < SizeY,
    nth0(Y, Input, Row),
    nth0(X, Row, N).
getNum(X, Y, SizeX, SizeY, _, edge):-
    X >= SizeX; Y >= SizeY.

cell(edge, T, R, B, L):- cell(0, T, R, B, L); cell(1, T, R, B, L).
cell(?, T, R, B, L):- cell(0, T, R, B, L); cell(1, T, R, B, L); cell(2, T, R, B, L); cell(3, T, R, B, L); cell(4, T, R, B, L).
cell(N, T, R, B, L):-
    side(T), side(R), side(B), side(L), N is T+R+B+L.

subSolve(X, Y, SizeX, SizeY, Input, [T, R, B, L]):-
    getNum(X, Y, SizeX, SizeY, Input, Num11),
    cell(Num11, T, R, B, L),

    X1 is X+1,
    getNum(X1, Y, SizeX, SizeY, Input, Num12),
    cell(Num12, _, _, _, R),

    Y1 is Y+1,
    getNum(X, Y1, SizeX, SizeY, Input, Num21),
    cell(Num21, B, _, _, _).
    %Crossing is (R+B + R3+B2) mod 2,
    %Crossing = 0.


/* doSolve(SizeX,SizeY,Input,Output) */
doSolve(SizeX, SizeY, Input, Solution):-
    subSolve(0, 0, SizeX, SizeY, Input, [T, R, B, L]),
    subSolve(1, 0, SizeX, SizeY, Input, [T2, R2, B2, L2]),
    subSolve(0, 1, SizeX, SizeY, Input, [T3, R3, B3, L3]),
    subSolve(1, 1, SizeX, SizeY, Input, [T4, R4, B4, L4]),
    B=T3, R=L2, R3=L4, B2=T4,
    Solution=[
        [T, T2],
        [L, R, R2],
        [B, B2],
        [L3, R3, R4],
        [B3, B4]
    ].

/********************* writing the result */
hChar(0, ' ').
hChar(1, '-').
vChar(0, ' ').
vChar(1, '|').

writeHorizontal([]):-
    write('+'), nl.
writeHorizontal([H|T]):-
    hChar(H, Char),
    write('+'), write(Char),
    writeHorizontal(T).

writeVertical([]):- nl.
writeVertical([H|T]):-
    vChar(H, Char),
    write(Char),
    length(T, TL),
    (TL > 0, write(' '); write('')),
    writeVertical(T).

writeSolution([]).
writeSolution([H|Solution]):-
    length(H, HL),
    Odd is HL mod 2,
    (Odd=0, writeHorizontal(H); writeVertical(H)),
    writeSolution(Solution).

writeFullOutput(S, X, Y):- write(X), write('x'), write(Y), nl, writeSolution(S).

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

