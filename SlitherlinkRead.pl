outputFile('./slither_solved.txt').
inputFile('./slither_unsolved.txt').

side(0).
side(1).

crossing(0).
crossing(2).

getNum(X, Y, SizeX, SizeY, Input, N):-
    X < SizeX, Y < SizeY,
    nth0(Y, Input, Row),
    nth0(X, Row, N).
getNum(X, Y, SizeX, SizeY, _, edge):-
    X >= SizeX; Y >= SizeY; X < 0; Y < 0.

cell(edge, T, R, B, L):- cell(0, T, R, B, L); cell(1, T, R, B, L).

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

subSolve(X, Y, SizeX, SizeY, Input, [T, R, B, L]):-
    getNum(X, Y, SizeX, SizeY, Input, Num),
    cell(Num, T, R, B, L),

    XS1 is X-1,
    getNum(XS1, Y, SizeX, SizeY, Input, NumXS),
    cell(NumXS, T2, R2, B2, L2),
    L=R2,

    YS1 is Y-1,
    getNum(X, YS1, SizeX, SizeY, Input, NumYS),
    cell(NumYS, T3, R3, B3, L3),
    T=B3,
    0 is (L3+T2+L+T) mod 2, % top left crossing

    XA1 is X+1,
    getNum(XA1, Y, SizeX, SizeY, Input, NumXA),
    cell(NumXA, T4, R4, B4, L4),
    R=L4,
    0 is (R3+T4+R+T) mod 2, % top right crossing

    YA1 is Y+1,
    getNum(X, YA1, SizeX, SizeY, Input, NumYA),
    cell(NumYA, T5, R5, B5, L5),
    B=T5,
    0 is (B2+L5+L+B) mod 2, % bottom left crossing
    0 is (B4+R5+R+B) mod 2. % bottom right crossing
    
    %Crossing is (R+B + R3+B2) mod 2,
    %Crossing = 0.


genCell(N, [T, R, B, L]):-
    cell(N, T, R, B, L).
/*
getSides(X, Y, SizeX, SizeY, Input, S):-
    X < SizeX, Y < SizeY,
    nth0(Y, Input, Row),
    nth0(X, Row, S).
getSides(X, Y, SizeX, SizeY, _, [T, R, B, L]):-
    X >= SizeX; Y >= SizeY; X < 0; Y < 0; cell(0, T, R, B, L); cell(1, T, R, B, L).


validate(X, Y, SizeX, SizeY, Board):-
    write(X), write('x'), write(Y), nl,
    X < SizeX, Y < SizeY,
    getSides(X, Y, SizeX, SizeY, Board, [T, R, B, L]),
    write([T, R, B, L]), nl,

    X1 is X+1,
    getSides(X1, Y, SizeX, SizeY, Board, [_, _, _, R]),

    Y1 is Y+1,
    getSides(X, Y1, SizeX, SizeY, Board, [B, _, _, _]),

    (
        X1 < SizeX, validate(X1, Y, SizeX, SizeY, Board);
        Y1 < SizeY, validate(0, Y1, SizeX, SizeY, Board)
    ).

validate(X, Y, SizeX, SizeY, Board).
*/

getSquare(X, Y, SizeX, SizeY, Board, Square):-
    X < SizeX-1, Y < SizeY-1,
    length(RowsBefore, Y),
    append(RowsBefore, [Row1, Row2|_], Board),
    length(ColsBefore1, X),
    length(ColsBefore2, X),
    append(ColsBefore1, [A, B|_], Row1),
    append(ColsBefore2, [C, D|_], Row2),
    Square = [[A, B], [C, D]].

validateSquare(X, Y, SizeX, SizeY, Board):-
    getSquare(X, Y, SizeX, SizeY, Board, [
        [[AT, AR, AB, AL], [BT, BR, BB, BL]],
        [[CT, CR, CB, CL], [DT, DR, DB, DL]]
    ]),
    AR=BL,
    AB=CT,
    BB=DT,
    CR=DL,
    0 is (AB+AR+CR+BB) mod 2, % center cross
    TopEdge is AT+BT+AR,
    TopEdge \= 3,
    RightEdge is BR+BB+DR,
    RightEdge \= 3,
    BottomEdge is CB+CR+DB,
    BottomEdge \= 3,
    LeftEdge is AL+CL+AB,
    LeftEdge \= 3.
    % todo: if at edge of board (X=0 etc), check that that edge has no 1-crosses.
    

/* doSolve(SizeX,SizeY,Input,Output) */
doSolve(SizeX, SizeY, Input, Solution):-
    maplist(maplist(genCell), Input, Board),
    validateSquare(0, 0, SizeX, SizeY, Board),
    validateSquare(1, 0, SizeX, SizeY, Board),
    validateSquare(0, 1, SizeX, SizeY, Board),
    validateSquare(1, 1, SizeX, SizeY, Board),
    writeSolution(Board).


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

