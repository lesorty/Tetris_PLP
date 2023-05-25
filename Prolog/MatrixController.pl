% MAtrixControler


% data Move = MoveNone | MoveLeft | MoveRight | MoveRotate | MoveDown | SuperDown | MoveSwap deriving Eq
% data BlockColor = Black | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving (Eq, Show)
% data Active = Prediction | Enable | Disable | None deriving Eq
% data Square = Square BlockColor Active deriving Eq
% data Tetromino = NullTetromino | Tetromino [(Int,Int)] BlockColor deriving Eq

% -- retorna a cor de um quadrado
% getColor :: Square -> BlockColor

% retorna se um quadrado está ativo
isActive('A').
isActive('B').
isActive('C').
isActive('D').
isActive('E').
isActive('F').
isActive('G').

isInactive('a').
isInactive('b').
isInactive('c').
isInactive('d').
isInactive('e').
isInactive('f').
isInactive('g').

getActiveLineColor([], 'none').
getActiveLineColor([Head|_], Color) :- 
    isActive(Head), 
    getColor(Head, Color).
getActiveLineColor([Head|Tail], Color) :-
    \+(isActive(Head)),
    getActiveLineColor(Tail, Color).

% -- retorna a cor dos quadrados que estão caindo
getActiveColor([Line|_], Color) :- 
    getActiveLineColor(Line, LineColor),
    LineColor =\= 'none',
    Color = LineColor.

getActiveColor([Line|Tail], Color) :-
    getActiveLineColor(Line, LineColor),
    LineColor =:= 'none',
    getActiveColor(Tail, Color).


toLower(X, Y) :- 
    char_code(X, Code), 
    print('Code: '), print(Code), nl,
    65 =< Code, Code =< 90,
    CodeLower is Code - 32, 
    print('CodeLower: '), print(CodeLower), nl,
    char_code(Y, CodeLower).

toLower(X, X) :- 
    char_code(X, Code), 
    97 =< Code, Code =< 122.

% cria uma lista com n elementos iguais a X
repeat(_, 0, []).
repeat(X, N, [X|L]) :- N > 0, N1 is N - 1, repeat(X, N1, L).

% -- retorna uma linha vazia
emptyLine(L) :- repeat('.', 10, L).

% retorna uma matriz vazia
emptyMatrix(M) :- 
    emptyLine(L),
    repeat(L, 25, M).

% -- retorna as coordenadas de um tetromino
% getTetrominoBlocks :: Tetromino -> [(Int, Int)]
getTetrominoBlocks([_|Blocks], Blocks).

% -- retorna a cor de um tetromino
% getTetrominoColor :: Tetromino -> BlockColor
getTetrominoColor([Color|_], Color).

getPos(Matrix, X, Y, Value) :- nth0(Y, Matrix, Line), nth0(X, Line, Value).

getActiveTetromino(Matrix, [Color | Coords]) :- 
    getActiveColor(Matrix, Color),
    findActiveMatrixIndexes(Matrix, 0, CoordsInMatrix),
    bringIndexesToZeroZero(CoordsInMatrix, Coords).

findActiveLineIndexes([], _, []).
findActiveLineIndexes([H|T], X, List) :- 
    isActive(H), 
    Next is X + 1, 
    findActiveLineIndexes(T, Next, TailList), 
    append([X], TailList, List).

findActiveLineIndexes([H|T], X, List) :- 
    \+(isActive(H)), 
    Next is X + 1, 
    findActiveLineIndexes(T, Next, List).

appendToAll([], _, []).
appendToAll([H|T], K, [NewH|NewT]) :- append(H, [K], NewH), appendToAll(T, K, NewT).

findActiveMatrixIndexes([], _, []).
findActiveMatrixIndexes([H|T], Y, List) :- 
    findActiveLineIndexes(H, 0, LineList),
    NextY is Y + 1,
    appendToAll(LineList, Y, NewList),
    findActiveMatrixIndexes(T, NextY, TailList),
    append(NewList, TailList, List).

% ------------ PIECE PERMISSION LOGIC ------------


% --retorna se um conjunto de blocos pode ser colocado na matrix.
canBePut(_, []).
canBePut(Matrix, [[X,Y]|T]) :- 
    getPos(Matrix, X, Y, Value),
    \+ (isInactive(Value)),
    canBePut(Matrix, T).


% -- retorna se é possível mover o tetromino para certa posição
canMoveTetromino(Matrix, Move) :- 
    getEndPos(Matrix, Move, List),
    canBePut(Matrix, List).



% ------------ PIECE MOVEMENT LOGIC ------------


% -- atualiza um elemento da matriz (magia!).
updateMatrixElement(Matrix, [X,Y], Value, NewMatrix) :- 
    nth0(Y, Matrix, Line),
    replace(Line, X, Value, NewLine),
    replace(Matrix, Y, NewLine, NewMatrix).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :- 
    I > 0, 
    I1 is I - 1, 
    replace(T, I1, X, R).

% -- atualiza uma lista de elementos da matriz
updateMatrixElements(Matrix, _, [], Matrix).
updateMatrixElements(Matrix, Value, [[X,Y]|T], NewMatrix) :- 
    updateMatrixElement(Matrix, [X,Y], Value, NewMatrix1),
    updateMatrixElements(NewMatrix1, Value, T, NewMatrix).



eraseIfActive(Start, End) :- isActive(Start), End = '.'.
eraseIfActive(X, X) :- \+ (isActive(X)).

% -- retorna a matriz sem blocos ativos
removeActiveBlocks([], []).
removeActiveBlocks([H|T], [NewH|NewT]) :- 
    removeActiveBlocks(T, NewT),
    maplist(eraseIfActive, H, NewH).

% -- retorna a matriz sem blocos de previsão
% removePrediction :: [[Square]] -> [[Square]]

% -- retorna as coordenadas finais de um movimento
getEndPos(Matrix, 'Left', List) :- 
    findActiveMatrixIndexes(Matrix, 0, Indexes),
    subtractFromAllLists(Indexes, [1,0], List).
getEndPos(Matrix, 'Right', List) :- 
    findActiveMatrixIndexes(Matrix, 0, Indexes),
    addToAllLists(Indexes, [1,0], List).
getEndPos(Matrix, 'Down', List) :-
    findActiveMatrixIndexes(Matrix, 0, Indexes),
    subtractFromAllLists(Indexes, [0,1], List).

% -- remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos(Matrix, List, NewMatrix) :- 
    getActiveColor(Matrix, Color),
    removeActiveBlocks(Matrix, ActivelessMatrix),
    updateMatrixElements(ActivelessMatrix, Color, List, NewMatrix).

% -- movimenta o Tetromino
% moveTetromino :: [[Square]] -> Move -> [[Square]]
moveTetromino(Matrix, Move, NewMatrix) :- 
    getEndPos(Matrix, Move, List),
    changeActiveBlocksPos(Matrix, List, NewMatrix).

% -- pega as coordenadas dos blocos de previsão
% getPredictionCoords :: [[Square]] -> [(Int, Int)]

% -- atualiza a previsão de acordo com a nova matriz
% updatePrediction :: [[Square]] -> [[Square]]

% -- derruba a peça instantaneamente
% fullFall :: [[Square]] -> [[Square]]
fullFall(Matrix, NewMatrix) :- 
    canMoveTetromino(Matrix, 'Down'),
    moveTetromino(Matrix, 'Down', PostFallMatrix),
    fullFall(PostFallMatrix, NewMatrix).
fullFall(Matrix, Matrix) :- \+(canMoveTetromino(Matrix, 'Down')).


% ------------ CLEAR MATRIX LOGIC ------------


% -- pega uma matriz e um índice. retorna se essa linha é clearável ou não
% canClearLine :: [Square] -> Bool
canClearLine([]).
canClearLine([H|T]) :- isDisabled(H), canClearLine(T).

removeFullLines([], []).
removeFullLines([H|T], NewMatrix) :- 
    canClearLine(H),
    removeFullLines(T, NewMatrix).
removeFullLines([H|T], [H|NewT]) :-
    \+(canClearLine(H)),
    removeFullLines(T, NewT).

padUntil25(Matrix, NewMatrix) :- 
    length(Matrix, Length),
    Length < 25,
    emptyLine(EmptyLine),
    append(Matrix, [EmptyLine], NewMatrix),
    padUntil25(NewMatrix, NewMatrix).

% -- pega uma matriz. retorna essa matriz com as linhas clearáveis limpas
clearMatrix(Matrix, [AddedScore | NewMatrix]) :- 
    removeFullLines(Matrix, RemovedLines),
    padUntil25(RemovedLines, NewMatrix),
    clearableCount(Matrix, AddedScore).

% -- retorna a quantidade de linhas que podem ser limpas de uma vez
% clearableCount :: [[Square]] -> Int
clearableCount(Matrix, Count) :- 
    findall(Y, (nth0(Y, Matrix, Line), canClearLine(Line)), List),
    length(List, Count).

% -- reinicia o ciclo chamando uma nova peça e limpando as linhas completas
goToNextCycle(Matrix, [AddedScore | NewMatrix]) :- 
    groundBlocks(Matrix, GroundedMatrix),
    clearMatrix(GroundedMatrix, [AddedScore | ClearedMatrix]),
    putRandomTetromino(ClearedMatrix, NewMatrix).
% ------------ GAME LOGIC ------------

getLast5Lines([H|T], NewMatrix) :- 
    length([H|T], Length),
    Length > 5,
    getLast5Lines(T, NewMatrix).    

isEmptyLine([]).
isEmptyLine([H|T]) :- H =:= '.', isEmptyLine(T).

isEmptyMatrix([]).
isEmptyMatrix([H|T]) :- isEmptyLine(H), isEmptyMatrix(T).

% --retorna true se tem algum bloco acima do limite da matriz, false caso contrário
isGameOver(Matrix) :- 
    getLast5Lines(Matrix, OutsideMatrix),
    \+(isEmptyMatrix(OutsideMatrix)).

% -- para cada quadrado da matriz, desativa caso seja ativo
%groundBlocks :: [[Square]] -> [[Square]]
groundBlocks([], []).
groundBlocks([H|T], [NewH|NewT]) :- 
    maplist(toLower, H, NewH),
    groundBlocks(T, NewT).

% -- gera uma peça aleatória
% getRandomTetromino :: Int -> Tetromino
getRandomTetromino(Tetromino) :- 
    %random(0, 7, Random),
    Random is 0,
    tetromino(Random, Tetromino).

tetromino(0, ['A',[0,0],[1,0],[2,0],[3,0]]).
tetromino(1, ['B',[0,0],[1,0],[0,-1],[0,-2]]).
tetromino(2, ['C',[0,0],[1,0],[0,-1],[1,-1]]).
tetromino(3, ['D',[0,0],[1,0],[1,-1],[1,-2]]).
tetromino(4, ['E',[0,0],[1,0],[2,0],[1,-1]]).
tetromino(5, ['F',[0,0],[1,0],[1,-1],[2,-1]]).
tetromino(6, ['G',[0,0],[0,-1],[1,-1],[1,-2]]).

% -- coloca em jogo uma peça aleatória
% putRandomTetromino :: [[Square]] -> Int -> [[Square]]
putRandomTetromino(Matrix, NewMatrix) :-
    getRandomTetromino(Tetromino),
    getTetrominoColor(Tetromino, Color),
    getTetrominoBlocks(Tetromino, Blocks),
    addToAllLists(Blocks, [3, 19], NewBlocks),
    raiseUntilAllowed(Matrix, NewBlocks, FinalCoords),
    updateMatrixElements(Matrix, Color, FinalCoords, NewMatrix).

% -- troca a peça atual por uma reserva
% swapTetromino :: [[Square]] -> Tetromino -> [[Square]]
swapTetromino(Matrix, Tetromino, NewMatrix) :- 
    getTetrominoBlocks(Tetromino, TetrominoBlocks),
    getTetrominoColor(Tetromino, TetrominoColor),
    findActiveMatrixIndexes(Matrix, 0, ActiveIndexes),
    baseDistance(ActiveIndexes, CurrentBaseDist),
    baseDistance(Tetromino, TetrominoBaseDist),
    removeActiveBlocks(Matrix, ClearedMatrix),
    subtractLists(CurrentBaseDist, TetrominoBaseDist, Shift),
    addToAllLists(TetrominoBlocks, Shift, ShiftedBlocks),
    raiseUntilAllowed(ClearedMatrix, ShiftedBlocks, FinalCoords),
    updateMatrixElements(ClearedMatrix, TetrominoColor, FinalCoords, NewMatrix).



% ------------ PIECE ROTATION LOGIC ------------


% -- verifica se a peça pode ser colocada apenas com um movimento lateral
canBePutWithSideMove(Matrix, Coords, Result) :- 
    canBePut(Matrix, Coords),
    Result = 'None', !.
canBePutWithSideMove(Matrix, Coords, Result) :- 
    \+ canBePut(Matrix, Coords),
    addToAllLists(Coords, [1, 0], ShiftedRight),
    canBePut(Matrix, ShiftedRight),
    Result = 'Right', !.
canBePutWithSideMove(Matrix, Coords, Result) :-
    \+ canBePut(Matrix, Coords),
    addToAllLists(Coords, [-1, 0], ShiftedLeft),
    canBePut(Matrix, ShiftedLeft),
    Result = 'Left', !.
canBePutWithSideMove(Matrix, Coords, 'No') :-
    \+ canBePut(Matrix, Coords),
    addToAllLists(Coords, [1, 0], ShiftedRight),
    \+canBePut(Matrix, ShiftedRight),
    addToAllLists(Coords, [-1, 0], ShiftedLeft),
    \+canBePut(Matrix, ShiftedLeft).


% -- pega uma matriz. retorna essa matriz com os blocos ativos rotacionados pra direita
rotateTetromino(Matrix, NewMatrix) :- 
    findActiveMatrixIndexes(Matrix, 0, ActiveIndexes),
    baseDistance(ActiveIndexes, BaseDistance),
    subtractLists(ActiveIndexes, BaseDistance, ZeroedIndexes),
    rotatePoints(ZeroedIndexes, RotatedZeroed),
    addToAllLists(RotatedZeroed, BaseDistance, ReturnedToPos),
    encloseCoords(ReturnedToPos, Enclosed),
    raiseUntilAllowed(Matrix, Enclosed, FinalPos),
    changeActiveBlocksPos(Matrix, FinalPos, NewMatrix).


% -- deixa o ponto superior esquerdo de um conjunto de coordenadas zerado. todas as outras relações se mantém
bringIndexesToZeroZero(Coords, NewCoords) :- 
    baseDistance(Coords, BaseDistance),
    subtractFromAllLists(Coords, BaseDistance, NewCoords).


% -- pega um conjunto de pontos na matriz. retorna, o ponto no top left
% baseDistance :: [(Int, Int)] -> (Int, Int)
baseDistance(Coords, [X, Y]) :- 
    minX(Coords, X),
    maxY(Coords, Y).

subtractLists([], [], []).
subtractLists([H1|T1], [H2|T2], [H3|T3]) :- 
    H3 is H1 - H2,
    subtractLists(T1, T2, T3).

% -- soma tuplas
addLists([], [], []).
addLists([H1|T1], [H2|T2], [H3|T3]) :- 
    H3 is H1 + H2,
    addLists(T1, T2, T3).

addToAllLists([], _, []).
addToAllLists([H|T], N, [EndH|EndT]) :- 
    addLists(H, N, EndH),
    addToAllLists(T, N, EndT).

subtractFromAllLists([], _, []).
subtractFromAllLists([H|T], N, [EndH|EndT]) :- 
    subtractLists(H, N, EndH),
    subtractFromAllLists(T, N, EndT).

rotatePoint([X, Y], [EndX, EndY]) :- 
    EndX is Y,
    EndY is -X.

% -- rotaciona um conjunto de pontos no sentido horário
rotatePoints(Points, Result) :- 
    maplist(rotatePoint, Points, PostRotation),
    minX(PostRotation, MinX),
    maxY(PostRotation, MaxY),
    addToAllLists(PostRotation, [-MinX, -MaxY], Result).

% --pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então retorna as novas coordenadas
raiseUntilAllowed(Matrix, Coords, NewCoords) :- 
    canBePutWithSideMove(Matrix, Coords, 'No'),
    print('No side move'), nl,
    addToAllLists(Coords, [0, 1], ShiftedCoords),
    raiseUntilAllowed(Matrix, ShiftedCoords, NewCoords), !.
raiseUntilAllowed(Matrix, Coords, NewCoords) :- 
    canBePutWithSideMove(Matrix, Coords, 'Right'),
    print('right move'), nl,
    addToAllLists(Coords, [1, 0], NewCoords), !.
raiseUntilAllowed(Matrix, Coords, NewCoords) :-
    canBePutWithSideMove(Matrix, Coords, 'Left'),
    print('Left move'), nl,
    addToAllLists(Coords, [-1, 0], NewCoords), !.
raiseUntilAllowed(Matrix, Coords, Coords) :- 
    canBePutWithSideMove(Matrix, Coords, 'None'),
    print('No move'), nl.



minX([], 99).
minX([(X,_)|T], Min) :- minX(T, TailMin), Min is min(X, TailMin).

maxX([], -99).
maxX([(X,_)|T], Max) :- maxX(T, TailMax), Max is max(X, TailMax).

minY([], 99).
minY([(_,Y)|T], Min) :- minY(T, TailMin), Min is min(Y, TailMin).

maxY([], -99).
maxY([(_,Y)|T], Max) :- maxY(T, TailMax), Max is max(Y, TailMax).

% -- coloca coordenadas dentro dos limites da matriz
encloseCoords([], []).
encloseCoords(Coords, NewCoords) :- 
    minX(Coords, MinX),
    minX < 0,
    addToAllLists(Coords, [-MinX, 0], NewCoords).
encloseCoords(Coords, NewCoords) :-
    maxX(Coords, MaxX),
    maxX > 9,
    Shift is -MaxX + 9,
    addToAllLists(Coords, [Shift, 0], NewCoords).

encloseCoords(Coords, Coords) :- 
    minX(Coords, MinX),
    maxX(Coords, MaxX),
    MinX >= 0,
    MaxX =< 9.
