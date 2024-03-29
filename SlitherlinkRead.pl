outputFile('./slither_solved.txt').
inputFile('./slither_unsolved.txt').

% A crossing / intersection can have 0 or 2 lines set to be valid.
crossing(0).
crossing(2).

% cell(N, Top, Right, Bottom, Left) represents a cell and all line configurations possible for that number.
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

% Helper predicates to get individual line segments around a cell / number.
getTop([T, _, _, _], T).
getRight([_, R, _, _], R).
getBottom([_, _, B, _], B).
getLeft([_, _, _, L], L).

% Find the value of the input board at coordinate, and return a possible line configuration around it.
getCell(X, Y, Input, Board, TRBL):-
    nth0(Y, Input, RowI), nth0(X, RowI, N), cell(N, T, R, B, L), TRBL = [T, R, B, L].

% Construct / find a valid 2x2 square (its internal edges will be valid, not necessarily for the board as a whole yet).
getSquare(X, Y, Input, Board, Square):-
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

% Helpers used below to validate the specific crossings of squares / edge intersections.
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

% For 1x1 boards, hard code solutions.
validate1x1([[0]], [[0], [0, 0], [0]]).
validate1x1([[4]], [[1], [1, 1], [1]]).
validate1x1([[?]], S):- validate1x1([[0]], S); validate1x1([[4]], S).

% For 2x2 boards, we don't need more than one "sub square" so validate it all at once.
validate2x2(Input, Board):-
    getSquare(0, 0, Input, Board, [
        [A, B],
        [C, D]
    ]),

    validateTopLeft([A, B, C, D]),
    validateTopRight([A, B, C, D]),
    validateBottomRight([A, B, C, D]),
    validateBottomLeft([A, B, C, D]),
    Board = [
        [A, B],
        [C, D]
    ].

% General solution for arbitrary board sizes bigger than 2x2.
validateSquare(X, Y, SizeX, SizeY, Input, Board):-
    getSquare(X, Y, Input, Board, [
        [A, B],
        [C, D]
    ]),

    % Depending on which sub square we are validating, we may need to validate the far edges or corners of the board itself.
    % An alternative solution would have been to extend the whole puzzle into negative coordinates, and beyond its width' height
    % and use a general solution, but we didn't think about that before this was already working.
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

    % "Insert" the valid square into the board solution by matching and constructing existing rows and columns with new data.
    % This is a bit hairy but without enforcing lengths for rows and columns like this we ended up with some partially filled
    % rows in the final solution. Didn't manage to find out why.
    length(RowsBefore, Y),
    append(RowsBefore, [Row1,Row2|_], Board),
    length(Cols1, X),
    append(Cols1, [A, B|_], Row1),
    length(Cols2, X),
    append(Cols2, [C, D|_], Row2),
    length(Row1, SizeX), length(Row2, SizeX).


% Recursively called by moving along the X-axis and constructing and validating sub squares. When the right side is reached,
% X is 0, and Y is increased, for the next recursive call. When next X (X1) and next Y are outside the board,
% we have a solution (though it may have multiple loops, which is checked for later).
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

% Currently the solution has lots of doubly represented lines (A's right is B's left etc). This predicate dedupes lines and gives
% a simpler representation for easier handling when writing / printing at the end.
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

% Finds a suitable start position for checking if we have multiple loops. This chooses the first set line it can find, and starts
% multi loop search from there (se next predicates)
checkMultiloops(RawBoard, X, Y):-
    simplifyBoard(RawBoard, Board), !,
    nth0(Y, Board, Row),
    nth0(X, Row, Line),
    (
        Line = 1, checkMultiloopsFrom(Board, X, Y);
        Line = 0, X1 is X+1, checkMultiloops(Board, X1, Y) % assume first row has some line set.
    ).

% Takes in a board, and sets line segment at coordinate to empty / 0.
nullifyLine(Board, X, Y, NewBoard):-
    length(RowsBefore, Y),
    append(RowsBefore, [Row|RowsAfter], Board),
    length(ColsBefore, X),

    append(ColsBefore, [_|ColsAfter], Row),
    append(ColsBefore, [0|ColsAfter], NewRow),
    append(RowsBefore, [NewRow], TopRows),
    append(TopRows, RowsAfter, NewBoard).

% Used to sum all line values in a row. We have a solution without multiloops if we've managed to walk the trail of the whole board
% and as a result only have 0s on the board.
sumList([], 0).
sumList([H|T], S):-
    sumList(T, S2),
    S is H+S2.

% Above predicate used on the board level.
checkNullBoard([]).
checkNullBoard([Row|Rest]):-
    sumList(Row, S),
    S is 0,
    checkNullBoard(Rest).

% Walks recursively along the link we've constructed (from start position found in checkMultiloops), and nullifies previous line.
% So a line "+-+-+" would start at the first segment and result in "+ +-+" etc.
checkMultiloopsFrom(Board, X, Y):-
    X >= 0, Y >= 0,
    length(Board, Rows),
    Y =< Rows,
    nth0(Y, Board, Row),
    length(Row, Cols),
    X =< Cols,
    nullifyLine(Board, X, Y, NewBoard),
    XS1 is X-1,
    XA1 is X+1,
    YS1 is Y-1,
    YA1 is Y+1,

    Odd is Y mod 2,
    (
        Odd = 0, nth0(Y, Board, Row1), nth0(XS1, Row1, 1), !, checkMultiloopsFrom(NewBoard, XS1, Y);
        Odd = 0, nth0(Y, Board, Row2), nth0(XA1, Row2, 1), !, checkMultiloopsFrom(NewBoard, XA1, Y);
        Odd = 0, nth0(YS1, Board, Row3), nth0(XA1, Row3, 1), !, checkMultiloopsFrom(NewBoard, XA1, YS1);
        Odd = 0, nth0(YS1, Board, Row3), nth0(X, Row3, 1), !, checkMultiloopsFrom(NewBoard, X, YS1);
        Odd = 0, nth0(YA1, Board, Row4), nth0(XA1, Row4, 1), !, checkMultiloopsFrom(NewBoard, XA1, YA1);
        Odd = 0, nth0(YA1, Board, Row4), nth0(X, Row4, 1), !, checkMultiloopsFrom(NewBoard, X, YA1);

        Odd = 1, YS2 is Y-2, nth0(YS2, Board, Row5), nth0(X, Row5, 1), !, checkMultiloopsFrom(NewBoard, X, YS2);
        Odd = 1, YA2 is Y+2, nth0(YA2, Board, Row6), nth0(X, Row6, 1), !, checkMultiloopsFrom(NewBoard, X, YA2);
        Odd = 1, nth0(YS1, Board, Row7), nth0(XS1, Row7, 1), !, checkMultiloopsFrom(NewBoard, XS1, YS1);
        Odd = 1, nth0(YS1, Board, Row8), nth0(X, Row8, 1), !, checkMultiloopsFrom(NewBoard, X, YS1);
        Odd = 1, nth0(YA1, Board, Row9), nth0(XS1, Row9, 1), !, checkMultiloopsFrom(NewBoard, XS1, YA1);
        Odd = 1, nth0(YA1, Board, Row10), nth0(X, Row10, 1), !, checkMultiloopsFrom(NewBoard, X, YA1);
        !, checkNullBoard(NewBoard)
    ).


doSolve(SizeX, SizeY, Input, Board):-
    (
        % Special handling for small boards
        SizeX = 1, SizeY = 1, validate1x1(Input, Board);
        SizeX = 2, SizeY = 2, validate2x2(Input, RawBoard), simplifyBoard(RawBoard, Board);
        % General solution for arbitrary board sizes
        partialSolve(0, 0, SizeX, SizeY, Input, RawBoard), checkMultiloops(RawBoard, 0, 0), simplifyBoard(RawBoard, Board)
    ).
    


% Write solution
hChar(0, ' ').
hChar(1, '-').
hChar(_, '?').
vChar(0, ' ').
vChar(1, '|').
vChar(_, '?').

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
    length(Rest, N),
    (
        0 is N mod 2, writeHorizontal(Row);
        1 is N mod 2, writeVertical(Row)
    ),
    writeSolution(Rest).

writeFullOutput(S, X, Y):- write(X), write('x'), write(Y), nl, writeSolution(S).

% From Andreas Prinz' starting file on Fronter:
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

