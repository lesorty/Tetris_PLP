% MAtrixControler


% data Move = MoveNone | MoveLeft | MoveRight | MoveRotate | MoveDown | SuperDown | MoveSwap deriving Eq
% data BlockColor = Black | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving (Eq, Show)
% data Active = Prediction | Enable | Disable | None deriving Eq
% data Square = Square BlockColor Active deriving Eq
% data Tetromino = NullTetromino | Tetromino [(Int,Int)] BlockColor deriving Eq

% retorna se 2 elementos sao iguais
isSame(X,X).

% retorna se um quadrado está ativo
isActive('A').
isActive('B').
isActive('C').
isActive('D').
isActive('E').
isActive('F').
isActive('G').

% retorna se um quadrado está inativo
isInactive('a').
isInactive('b').
isInactive('c').
isInactive('d').
isInactive('e').
isInactive('f').
isInactive('g').

%  retorna a cor ativa de uma linha
getActiveLineColor([], 'none').
getActiveLineColor([Head|_], Head) :- 
    isActive(Head).
getActiveLineColor([Head|Tail], Color) :-
    \+(isActive(Head)),
    getActiveLineColor(Tail, Color).

%  retorna a cor dos quadrados que estão caindo
getActiveColor([Line|_], Color) :- 
    getActiveLineColor(Line, LineColor),
    \+ LineColor = 'none',
    Color = LineColor.

getActiveColor([Line|Tail], Color) :-
    getActiveLineColor(Line, LineColor),
    LineColor = 'none',
    getActiveColor(Tail, Color).

% converte caractere para minusculo
toLower(X, Y) :- 
    char_code(X, Code), 
    65 =< Code, Code =< 90,
    CodeLower is Code + 32, 
    char_code(Y, CodeLower).

toLower(X, X) :- 
    char_code(X, Code), 
    (Code < 65; Code > 90).

% converte caractere para maiusculo
toUpper(X, Y) :- 
    char_code(X, Code), 
    97 =< Code, Code =< 122,
    CodeUpper is Code - 32, 
    char_code(Y, CodeUpper).

toUpper(X, X) :-
    char_code(X, Code), 
    (Code < 97; Code > 122).

% converte uma string para minusculo
toLowerString([], []).
toLowerString([H|T], [NewH|NewT]) :- 
    toLower(H, NewH), 
    toLowerString(T, NewT).

% cria uma lista com n elementos iguais a X
repeat(_, 0, []).
repeat(X, N, [X|L]) :- N > 0, N1 is N - 1, repeat(X, N1, L).

% retorna uma linha vazia
emptyLine(L) :- repeat('.', 10, L).

% retorna uma matriz vazia
emptyMatrix(M) :- 
    emptyLine(L),
    repeat(L, 25, M).

% retorna uma matriz vazia de tamanho 2x4
emptySmallMatrix([['.','.','.','.'], ['.', '.', '.', '.']]).


% -- retorna as coordenadas de um tetromino
getTetrominoBlocks([_|Blocks], Blocks).

% -- retorna a cor de um tetromino
getTetrominoColor([Color|_], Color).

% retorna o elemento de uma matriz em uma certa coordenada
getPos(Matrix, X, Y, Value) :- nth0(Y, Matrix, Line), nth0(X, Line, Value).

% retorna o tetromino ativo
getActiveTetromino(Matrix, [Color | FinalCoords]) :- 
    getActiveColor(Matrix, Color),
    findActiveMatrixIndexes(Matrix, 0, CoordsInMatrix),
    bringIndexesToZeroZero(CoordsInMatrix, Coords),
    minY(Coords, MinY), 
    (MinY < -1 -> 
        rotatePoints(Coords, CoordsRotated), 
        bringIndexesToZeroZero(CoordsRotated, FinalCoords)
        ; 
        FinalCoords = Coords).

% retorna os indices ativos numa linha
findActiveLineIndexes([], _, []).
findActiveLineIndexes([H|T], X, List) :-
    isActive(H),
    Next is X + 1, 
    findActiveLineIndexes(T, Next, TailList), 
    append([X], TailList, List).

% retorna os indices ativos numa matriz
findActiveLineIndexes([H|T], X, List) :- 
    \+(isActive(H)), 
    Next is X + 1, 
    findActiveLineIndexes(T, Next, List).

% da append de um elemento a todos os elementos de uma lista
appendToAll([], _, []).
appendToAll([H|T], K, [NewH|NewT]) :- 
    append([H], [K], NewH), 
    appendToAll(T, K, NewT).

% retorna as coordenadas do tetromino ativo
findActiveMatrixIndexes([], _, []).
findActiveMatrixIndexes([H|T], Y, List) :- 
    findActiveLineIndexes(H, 0, LineList),
    NextY is Y + 1,
    appendToAll(LineList, Y, NewList),
    findActiveMatrixIndexes(T, NextY, TailList),
    append(NewList, TailList, List).

% ------------ PIECE PERMISSION LOGIC ------------


% retorna se um conjunto de blocos pode ser colocado na matrix.
canBePut(_, []).
canBePut(Matrix, [[X,Y]|T]) :- 
    getPos(Matrix, X, Y, Value),
    \+ (isInactive(Value)),
    canBePut(Matrix, T).


% retorna se é possível mover o tetromino numa certa direcao
canMoveTetromino(Matrix, Move) :- 
    getEndPos(Matrix, Move, List),
    canBePut(Matrix, List).



% ------------ PIECE MOVEMENT LOGIC ------------


% atualiza um elemento da matriz.
updateMatrixElement(Matrix, [X,Y], Value, NewMatrix) :- 
    nth0(Y, Matrix, Line),
    replace(Line, X, Value, NewLine),
    replace(Matrix, Y, NewLine, NewMatrix).

% substitui um elemento de uma lista
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :- 
    I > 0, 
    I1 is I - 1, 
    replace(T, I1, X, R).

%  atualiza uma lista de elementos da matriz
updateMatrixElements(Matrix, Value, [[X,Y]|T], NewMatrix) :- 
    updateMatrixElement(Matrix, [X,Y], Value, NewMatrix1),
    updateMatrixElements(NewMatrix1, Value, T, NewMatrix).

updateMatrixElements(Matrix, _, [], Matrix).    

    
% se um caractere é ativo, apaga ele
eraseIfActive(Start, End) :- isActive(Start), End = '.'.
eraseIfActive(X, X) :- \+ (isActive(X)).

% retorna a linha sem blocos ativos
removeActiveLineBlocks([], []).
removeActiveLineBlocks([H|T], [NewH|NewT]) :- 
    eraseIfActive(H, NewH),
    removeActiveLineBlocks(T, NewT).

% -- retorna a matriz sem blocos ativos
removeActiveMatrixBlocks([], []).
removeActiveMatrixBlocks([H|T], [NewH|NewT]) :- 
    removeActiveLineBlocks(H, NewH),
    removeActiveMatrixBlocks(T, NewT).

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

% remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos(Matrix, List, NewMatrix) :- 
    getActiveColor(Matrix, Color),
    removeActiveMatrixBlocks(Matrix, ActivelessMatrix),
    updateMatrixElements(ActivelessMatrix, Color, List, NewMatrix).

% -- movimenta o Tetromino ativo
moveTetromino(Matrix, Move, NewMatrix) :- 
    getEndPos(Matrix, Move, List),
    changeActiveBlocksPos(Matrix, List, NewMatrix).


% derruba a peça instantaneamente
fullFall(Matrix, NewMatrix) :- 
    canMoveTetromino(Matrix, 'Down'),
    moveTetromino(Matrix, 'Down', PostFallMatrix),
    fullFall(PostFallMatrix, NewMatrix).
fullFall(Matrix, Matrix) :- \+(canMoveTetromino(Matrix, 'Down')).

% ------------ CLEAR MATRIX LOGIC ------------


% pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine([]).
canClearLine([H|T]) :- isInactive(H), canClearLine(T).

% remove todas as linhas clearáveis de uma matriz
removeFullLines([], []).
removeFullLines([H|T], NewMatrix) :- 
    canClearLine(H),
    removeFullLines(T, NewMatrix).
removeFullLines([H|T], [H|NewT]) :-
    \+(canClearLine(H)),
    removeFullLines(T, NewT).

% bota linhas vazias numa lista até que seu tamanho seja 25
padUntil25(Matrix, NewMatrix) :- 
    length(Matrix, Length),
    Length < 25,
    emptyLine(EmptyLine),
    append(Matrix, [EmptyLine], PaddedMatrix),
    padUntil25(PaddedMatrix, NewMatrix).
padUntil25(Matrix, Matrix) :-
    length(Matrix, 25).


% -- pega uma matriz. retorna essa matriz com as linhas clearáveis limpas, e a quantidade de pontos adicionados
clearMatrix(Matrix, [AddedScore | NewMatrix]) :- 
    removeFullLines(Matrix, RemovedLines),
    padUntil25(RemovedLines, NewMatrix),
    clearableCount(Matrix, ClearedLineAmount),
    pointsForClear(ClearedLineAmount, AddedScore).

% -- retorna a quantidade de linhas que podem ser limpas de uma vez
clearableCount(Matrix, Count) :- 
    findall(Y, (nth0(Y, Matrix, Line), canClearLine(Line)), List),
    length(List, Count).

% -- reinicia o ciclo chamando uma nova peça e limpando as linhas completas
goToNextCycle(Matrix, Tetromino, [AddedScore | NewMatrix]) :-
    groundBlocks(Matrix, GroundedMatrix),
    clearMatrix(GroundedMatrix, [AddedScore | ClearedMatrix]),
    putNextTetromino(ClearedMatrix, Tetromino, NewMatrix).
% ------------ GAME LOGIC ------------

% retorna as ultimas 5 linhas da matriz
getLast5Lines([H|T], NewMatrix) :- 
    length([H|T], Length),
    Length > 5,
    getLast5Lines(T, NewMatrix).    

getLast5Lines(Matrix, Matrix) :- 
    length(Matrix, Length),
    Length =< 5.

% retorna a matriz sem as ultimas 5 linhas
dropLast5Lines([H|T], []) :- 
    length([H|T], 5).

dropLast5Lines([H|T], [H|NewT]) :-
    length([H|T], Length),
    Length > 5,
    dropLast5Lines(T, NewT).

% e verdadeiro se a linha for vazia
isEmptyLine([]).
isEmptyLine([H|T]) :- H = '.', isEmptyLine(T).

% e verdadeiro se a matriz for vazia
isEmptyMatrix([]).
isEmptyMatrix([H|T]) :- isEmptyLine(H), isEmptyMatrix(T).

% e verdadeiro se nao tiver nenhum quadrado fora dos limites da matriz
isGameOver(Matrix) :- 
    getLast5Lines(Matrix, OutsideMatrix),
    \+(isEmptyMatrix(OutsideMatrix)).

% para cada quadrado de uma linha, desativa caso seja ativo
groundLine([], []).
groundLine([H|T], [NewH|NewT]) :- 
    toLower(H, NewH),
    groundLine(T, NewT).

% para cada linha da matriz, desativa os quadrados ativos
groundBlocks([], []).
groundBlocks([H|T], [NewH|NewT]) :- 
    groundLine(H, NewH),
    groundBlocks(T, NewT).

% retorna um tetromino aleatorio
getRandomTetromino(Tetromino) :- 
    random(0, 7, Random),
    tetromino(Random, Tetromino).

% retorna um tetromino baseado no seu indice
tetromino(0, ['A',[0,0],[1,0],[2,0],[3,0]]).
tetromino(1, ['B',[0,0],[1,0],[2,0],[2,-1]]).
tetromino(2, ['C',[0,0],[1,0],[0,-1],[1,-1]]).
tetromino(3, ['D',[0,-1],[1,-1],[2,0],[2,-1]]).
tetromino(4, ['E',[0,0],[1,0],[2,0],[1,-1]]).
tetromino(5, ['F',[0,0],[1,0],[1,-1],[2,-1]]).
tetromino(6, ['G',[0,-1],[1,0],[1,-1],[2,0]]).

% coloca um tetromino no jogo
putNextTetromino(Matrix, Tetromino, NewMatrix) :-
    getTetrominoColor(Tetromino, Color),
    getTetrominoBlocks(Tetromino, Blocks),
    addToAllLists(Blocks, [3, 19], NewBlocks),
    raiseUntilAllowed(Matrix, NewBlocks, FinalCoords),
    updateMatrixElements(Matrix, Color, FinalCoords, NewMatrix).    

% troca um tetromino por um novo
swapTetromino(Matrix, Tetromino, NewMatrix) :- 
    getTetrominoBlocks(Tetromino, TetrominoBlocks),
    getTetrominoColor(Tetromino, TetrominoColor),
    findActiveMatrixIndexes(Matrix, 0, ActiveIndexes),
    baseDistance(ActiveIndexes, CurrentBaseDist),
    [_ | TetrominoBlocks] = Tetromino,
    baseDistance(TetrominoBlocks, TetrominoBaseDist),
    removeActiveMatrixBlocks(Matrix, ClearedMatrix),
    subtractLists(CurrentBaseDist, TetrominoBaseDist, Shift),
    addToAllLists(TetrominoBlocks, Shift, ShiftedBlocks),
    encloseCoords(ShiftedBlocks, EnclosedShiftedBlocks),
    raiseUntilAllowed(ClearedMatrix, EnclosedShiftedBlocks, FinalCoords),
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

% se um caractere eh de previsao, apaga ele
eraseIfPrediction(Square, NewSquare) :- 
    Square = '#', NewSquare = '.'.
eraseIfPrediction(Square, Square) :- 
    Square \= '#'.

% remove as previsoes da linha
removeLinePrediction([], []).
removeLinePrediction([H|T], [NewH|NewT]) :- 
    eraseIfPrediction(H, NewH),
    removeLinePrediction(T, NewT).

% remove as previsoes da matriz
removePrediction([], []).
removePrediction([H|T], [NewH|NewT]) :- 
    removeLinePrediction(H, NewH),
    removePrediction(T, NewT).

% remove uma coordenada de uma lista de coordenadas se ela nao esta vazia na matriz
removeFromListIfCoordNotEmpty(_, [], []).

removeFromListIfCoordNotEmpty(Matrix, [[X, Y]|T], [[X, Y]|NewT]) :- 
    getPos(Matrix, X,Y, Square),
    Square == '.',
    removeFromListIfCoordNotEmpty(Matrix, T, NewT).

removeFromListIfCoordNotEmpty(Matrix, [[X,Y]|T], NewT) :-
    getPos(Matrix, X,Y, Square),
    Square \= '.',
    removeFromListIfCoordNotEmpty(Matrix, T, NewT).

% pega uma matriz. retorna uma lista de coordenadas de previsao
getPredictionCoords(Matrix, Coords) :- 
    canMoveTetromino(Matrix, 'Down'),
    moveTetromino(Matrix, 'Down', NewMatrix),
    getPredictionCoords(NewMatrix, Coords).

getPredictionCoords(Matrix, Coords) :-
    \+ canMoveTetromino(Matrix, 'Down'),
    findActiveMatrixIndexes(Matrix, 0, Coords).

% atualiza a matriz com as previsoes
updatePrediction(Matrix, NewMatrix) :-
    removePrediction(Matrix, ClearedMatrix),
    getPredictionCoords(Matrix, Coords),
    removeFromListIfCoordNotEmpty(ClearedMatrix, Coords, NewCoords),
    updateMatrixElements(ClearedMatrix, '#', NewCoords, NewMatrix).


% -- pega uma matriz. retorna essa matriz com os blocos ativos rotacionados pra direita
rotateTetromino(Matrix, NewMatrix) :- 
    findActiveMatrixIndexes(Matrix, 0, ActiveIndexes),
    baseDistance(ActiveIndexes, BaseDistance),
    subtractFromAllLists(ActiveIndexes, BaseDistance, ZeroedIndexes),
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

% subtrai listas
subtractLists([], [], []).
subtractLists([H1|T1], [H2|T2], [H3|T3]) :-
    H3 is H1 - H2,
    subtractLists(T1, T2, T3).

% -- soma listas
addLists([], [], []).
addLists([H1|T1], [H2|T2], [H3|T3]) :- 
    H3 is H1 + H2,
    addLists(T1, T2, T3).

% adiciona lista a todas as listas de uma lista de listas!!
addToAllLists([], _, []).
addToAllLists([H|T], N, [EndH|EndT]) :- 
    addLists(H, N, EndH),
    addToAllLists(T, N, EndT).

% subtrai lista de todas as listas de uma lista de listas
subtractFromAllLists([], _, []).
subtractFromAllLists([H|T], N, [EndH|EndT]) :- 
    subtractLists(H, N, EndH),
    subtractFromAllLists(T, N, EndT).

% rotaciona ponto no sentido horário
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
    addToAllLists(Coords, [0, 1], ShiftedCoords),
    raiseUntilAllowed(Matrix, ShiftedCoords, NewCoords), !.
raiseUntilAllowed(Matrix, Coords, NewCoords) :-     
    canBePutWithSideMove(Matrix, Coords, 'Right'),
    addToAllLists(Coords, [1, 0], NewCoords), !.

raiseUntilAllowed(Matrix, Coords, NewCoords) :-
    canBePutWithSideMove(Matrix, Coords, 'Left'),
    addToAllLists(Coords, [-1, 0], NewCoords), !.
raiseUntilAllowed(Matrix, Coords, Coords) :- 
    canBePutWithSideMove(Matrix, Coords, 'None').

% retorna o menor X de uma lista de coordenadas
minX([], 99).
minX([[X,_]|T], Min) :- 
    minX(T, TailMin), 
    Min is min(X, TailMin).

% retorna o maior X de uma lista de coordenadas
maxX([], -99).
maxX([[X,_]|T], Max) :- maxX(T, TailMax), Max is max(X, TailMax).

% retorna o menor Y de uma lista de coordenadas
minY([], 99).
minY([[_,Y]|T], Min) :- minY(T, TailMin), Min is min(Y, TailMin).

% retorna o maior Y de uma lista de coordenadas
maxY([], -99).
maxY([[_,Y]|T], Max) :- maxY(T, TailMax), Max is max(Y, TailMax).

% -- coloca coordenadas dentro dos limites da matriz
encloseCoords([], []).
encloseCoords(Coords, NewCoords) :- 
    minX(Coords, MinX),
    MinX < 0,
    addToAllLists(Coords, [-MinX, 0], NewCoords).
encloseCoords(Coords, NewCoords) :-
    maxX(Coords, MaxX),
    MaxX > 9,
    Shift is -MaxX + 9,
    addToAllLists(Coords, [Shift, 0], NewCoords).

encloseCoords(Coords, Coords) :- 
    minX(Coords, MinX),
    maxX(Coords, MaxX),
    MinX >= 0,
    MaxX =< 9.
