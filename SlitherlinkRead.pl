outputFile('./slither_solved.txt').
inputFile('./slither_unsolved.txt').

cell(0, 0, 0, 0, 0).

cell(1, 1, 0, 0, 0).
cell(1, 0, 1, 0, 0).
cell(1, 0, 0, 1, 0).
cell(1, 0, 0, 0, 1).

cell(2, 1, 1, 0, 0).
cell(2, 1, 0, 1, 0).
cell(2, 1, 0, 0, 1).
cell(2, 0, 1, 1, 0).
cell(2, 0, 1, 0, 1).
cell(2, 0, 0, 1, 1).

cell(3, 1, 1, 1, 0).
cell(3, 1, 1, 0, 1).
cell(3, 1, 0, 1, 1).
cell(3, 0, 1, 1, 1).

cell(4, 1, 1, 1, 1).

cell(?, T, R, B, L):- cell(0, T, R, B, L); cell(1, T, R, B, L); cell(2, T, R, B, L); cell(3, T, R, B, L); cell(4, T, R, B, L).


genCell(N, [T, R, B, L]):-
    cell(N, T, R, B, L).


getSquare(X, Y, SizeX, SizeY, Board, Square):-
    X < SizeX-1, Y < SizeY-1,
    length(RowsBefore, Y),
    append(RowsBefore, [Row1, Row2|_], Board),
    length(ColsBefore1, X),
    length(ColsBefore2, X),
    append(ColsBefore1, [A, B|_], Row1),
    append(ColsBefore2, [C, D|_], Row2),
    Square = [
        [A, B],
        [C, D]
    ].

validateSquare(X, Y, SizeX, SizeY, Board):-
    getSquare(X, Y, SizeX, SizeY, Board, [
        [[AT, AR, AB, AL], [BT, BR, BB, BL]],
        [[CT, CR, CB, CL], [DT, DR, DB, DL]]
    ]),
    write(X), write('x'), write(Y), nl,
    AR=BL, AB=CT, BB=DT, CR=DL,
    0 is (AB+AR+CR+BB) mod 2, % center cross
    /*TopEdge is AT+BT+AR,
    TopEdge \= 3,
    RightEdge is BR+BB+DR,
    RightEdge \= 3,
    BottomEdge is CB+CR+DB,
    BottomEdge \= 3,
    LeftEdge is AL+CL+AB,
    LeftEdge \= 3,*/

    SizeX1 is SizeX-1,
    SizeY1 is SizeY-1,
    (
        % todo: get all combinations here, fails on 1x1 and 2x2, but works on >= 3x3.
        X = 0, Y = 0, 0 is (AL+AB+CL) mod 2, 0 is (AT+AR+BT) mod 2;
        X = SizeX1, Y = SizeY1, 0 is (BR+BB+DR) mod 2, 0 is (CB+CR+DB) mod 2;
        X = 0, Y \= 0, 0 is (AL+AB+CL) mod 2;
        Y = 0, X \= 0, 0 is (AT+AR+BT) mod 2;
        X = SizeX1, Y \= SizeY1, 0 is (BR+BB+DR) mod 2;
        Y = SizeY1, X \= SizeX1, 0 is (CB+CR+DB) mod 2;
        X > 0, Y > 0, X < SizeX1, Y < SizeY1
    ).

partialSolve(X, Y, SizeX, SizeY, Board):-
    SizeX1 is SizeX-1,
    SizeY1 is SizeY-1,
    X1 is X+1,
    Y1 is Y+1,

    validateSquare(X, Y, SizeX, SizeY, Board),

    (
        X1 < SizeX1, partialSolve(X1, Y, SizeX, SizeY, Board);
        Y1 < SizeY1, partialSolve(0, Y1, SizeX, SizeY, Board);
        X1 >= SizeX1, Y1 >= SizeY1
    ).
    

/* doSolve(SizeX,SizeY,Input,Output) */
doSolve(SizeX, SizeY, Input, Board):-
    maplist(maplist(genCell), Input, Board),
    partialSolve(0, 0, SizeX, SizeY, Board).


/********************* writing the result */
hChar(0, ' ').
hChar(1, '-').
vChar(0, ' ').
vChar(1, '|').

getTop([T, _, _, _], T).
getRight([_, R, _, _], R).
getBottom([_, _, B, _], B).
getLeft([_, _, _, L], L).

writeHorizontal([]):-
    write('+'), nl.
writeHorizontal([H|T]):-
    hChar(H, Char),
    write('+'), write(Char),
    writeHorizontal(T).

writeVertical([]).
writeVertical([H|T]):-
    vChar(H, Char),
    write(Char),
    length(T, TL),
    (TL > 0, write(' '); write('')),
    writeVertical(T).

writeSolution([Row|Rest]):-
    maplist(getTop, Row, Tops),
    writeHorizontal(Tops),
    maplist(getLeft, Row, Lefts),
    writeVertical(Lefts),

    % last | if set.
    last(Row, [_, R, _, _]),
    vChar(R, VChar),
    write(' '), write(VChar), nl,

    (
        length(Rest, 0), maplist(getBottom, Row, Bottoms), writeHorizontal(Bottoms);
        writeSolution(Rest)
    ).

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

