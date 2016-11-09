outputFile('./slither_solved.txt').
inputFile('./slither_unsolved.txt').

line(0).
line(1).

% a crossing / intersection can have 0 or 2 lines set to be valid.
crossing(0).
crossing(2).

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
cell(?, T, R, B, L):- cell(1, T, R, B, L); cell(2, T, R, B, L); cell(0, T, R, B, L); cell(3, T, R, B, L); cell(4, T, R, B, L).

genCell(N, [T, R, B, L]):-
    cell(N, T, R, B, L).

getCell(X, Y, Input, Board, TRBL):-
    nth0(Y, Input, RowI), nth0(X, RowI, N), genCell(N, TRBL).

getSquare(X, Y, SizeX, SizeY, Input, Board, Square):-
    getCell(X, Y, Input, Board, [AT, AR, AB, AL]),

    X1 is X+1,
    getCell(X1, Y, Input, Board, [BT, BR, BB, BL]),
    AR=BL, 
    TopEdge is AT+BT+AR,
    TopEdge \= 3,

    Y1 is Y+1,
    getCell(X, Y1, Input, Board, [CT, CR, CB, CL]),
    AB=CT,
    LeftEdge is AL+CL+AB,
    LeftEdge \= 3,

    Crossing is (AB+AR+CR+BB), % center cross
    crossing(Crossing),

    getCell(X1, Y1, Input, Board, [DT, DR, DB, DL]),
    BB=DT, CR=DL,

    RightEdge is BR+BB+DR,
    RightEdge \= 3,
    BottomEdge is CB+CR+DB,
    BottomEdge \= 3,

    Square = [
        [[AT, AR, AB, AL], [BT, BR, BB, BL]],
        [[CT, CR, CB, CL], [DT, DR, DB, DL]]
    ].

validateTop([A, B, _, _]):-
    A = [AT, AR, _, _],
    B = [BT, _, _, _],
    Crossing is (AT+AR+BT),
    crossing(Crossing).
validateRight([_, B, _, D]):-
    B = [_, BR, BB, _],
    D = [_, DR, _, _],
    Crossing is (BR+BB+DR),
    crossing(Crossing).
validateBottom([_, _, C, D]):-
    C = [_, CR, CB, _],
    D = [_, _, DB, _],
    Crossing is (CB+CR+DB),
    crossing(Crossing).
validateLeft([A, _, C, _]):-
    A = [_, _, AB, AL],
    C = [_, _, _, CL],
    Crossing is (AL+AB+CL),
    crossing(Crossing).


validateTopLeft([A, B, C, D]):-
    validateTop([A, B, C, D]),
    validateLeft([A, B, C, D]),
    A = [AT, _, _, AL],
    Crossing is (AT+AL),
    crossing(Crossing).
validateTopRight([A, B, C, D]):-
    validateTop([A, B, C, D]),
    validateRight([A, B, C, D]),
    B = [BT, BR, _, _],
    Crossing is (BT+BR),
    crossing(Crossing).
validateBottomRight([A, B, C, D]):-
    validateBottom([A, B, C, D]),
    validateRight([A, B, C, D]),
    D = [_, DR, DB, _],
    Crossing is (DR+DB),
    crossing(Crossing).
validateBottomLeft([A, B, C, D]):-
    validateBottom([A, B, C, D]),
    validateLeft([A, B, C, D]),
    C = [_, _, CB, CL],
    Crossing is (CB+CL),
    crossing(Crossing).


validateSquare(X, Y, SizeX, SizeY, Input, Board):-
    getSquare(X, Y, SizeX, SizeY, Input, Board, [
        [A, B],
        [C, D]
    ]),

    SizeX1 is SizeX-2,
    SizeY1 is SizeY-2,
    (
        X > 0,       Y > 0,       X < SizeX1, Y < SizeY1;
        X = 0,       Y = 0,       validateTopLeft([A, B, C, D]);
        X = SizeX1,  Y = 0,       validateTopRight([A, B, C, D]);
        X = SizeX1,  Y = SizeY1,  validateBottomRight([A, B, C, D]);
        X = 0,       Y = SizeY1,  validateBottomLeft([A, B, C, D]);
        X \= 0,      X \= SizeX1, Y = 0,       validateTop([A, B, C, D]);
        X = SizeX1,  Y \= 0,      Y \= SizeY1, validateRight([A, B, C, D]);
        X \= 0,      X \= SizeX1, Y = SizeY1,  validateBottom([A, B, C, D]);
        X = 0,       Y \= 0,      Y \= SizeY1,  validateLeft([A, B, C, D])
    ),

    length(RowsBefore, Y),
    append(RowsBefore, [Row1,Row2|_], Board),
    length(Cols1, X),
    append(Cols1, [A, B|_], Row1),
    length(Cols2, X),
    append(Cols2, [C, D|_], Row2),
    length(Row1, SizeX), length(Row2, SizeX).


partialSolve(X, Y, SizeX, SizeY, Input, Board):-
    SizeX1 is SizeX-1,
    SizeY1 is SizeY-1,
    X1 is X+1,
    Y1 is Y+1,

    validateSquare(X, Y, SizeX, SizeY, Input, Board),

    (
        X1 < SizeX1, partialSolve(X1, Y, SizeX, SizeY, Input, Board);
        X1 = SizeX1, Y1 < SizeY1, partialSolve(0, Y1, SizeX, SizeY, Input, Board);
        X1 >= SizeX1, Y1 >= SizeY1
    ).


simplifyBoard([], []).
simplifyBoard([Row|Rest], NewSimpleBoard):-
    simplifyBoard(Rest, SimpleBoard),
    maplist(getTop, Row, Tops),
    maplist(getLeft, Row, Lefts),

    % last | if set.
    last(Row, [_, R, _, _]),

    append(Lefts, [R], Verticals),

    (
        length(Rest, 0), maplist(getBottom, Row, Bottoms), append([Tops, Verticals, Bottoms], SimpleBoard, NewSimpleBoard);
        append([Tops, Verticals], SimpleBoard, NewSimpleBoard)
    ).
    

doSolve(SizeX, SizeY, Input, Board):-
    partialSolve(0, 0, SizeX, SizeY, Input, RawBoard),
    simplifyBoard(RawBoard, Board).


/********************* writing the result */
hChar(0, ' ').
hChar(1, '-').
hChar(_, '?').
vChar(0, ' ').
vChar(1, '|').
vChar(_, '?').

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

writeVertical([]):- nl.
writeVertical([H|T]):-
    vChar(H, Char),
    write(Char),
    length(T, TL),
    (TL > 0, write(' '); write('')),
    writeVertical(T).

writeSolution([]).
writeSolution([Row|Rest]):-
    length(Row, N),
    (
        0 is N mod 2, writeHorizontal(Row);
        1 is N mod 2, writeVertical(Row)
    ),
    writeSolution(Rest).

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

