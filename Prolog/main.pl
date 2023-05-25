%change_directory('D:/Program Files (x86)/Tetris_PLP/Prolog').

% -- define os valores do jogo
% data GameState = GameState {
%     matrix :: [[Square]],
%     score :: Int,
%     droppedPieces :: Int,
%     pieceSwap :: Tetromino,
%     }

% -- define as condições iniciais
% newGameState :: GameState
:- include('MatrixController.pl').
:- include('Screen.pl').

newGameState(GameState) :-
    emptyMatrix(Matrix),
    getRandomTetromino(SwapTetromino),
    putRandomTetromino(Matrix, NewMatrix),
    GameState = [NewMatrix, 0, 0, SwapTetromino].

% -- aplica um movimento numa matriz
% applyMove :: GameState -> Move -> GameState
applyMove(GameState, 'Left', NewGameState) :-
    [Matrix, Score, DroppedPieces, PieceSwap] = GameState,
    canMoveTetromino(Matrix, 'Left'),
    moveTetromino(Matrix, 'Left', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, PieceSwap], !.

applyMove(GameState, 'Right', NewGameState) :-
    [Matrix, Score, DroppedPieces, PieceSwap] = GameState,
    canMoveTetromino(Matrix, 'Right'),
    moveTetromino(Matrix, 'Right', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, PieceSwap], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, PieceSwap] = GameState,
    canMoveTetromino(Matrix, 'Down'),
    moveTetromino(Matrix, 'Down', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, PieceSwap], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, PieceSwap] = GameState,
    \+(canMoveTetromino(Matrix, 'Down')),
    goToNextCycle(Matrix, [AddedScore | NewMatrix]),
    NewDroppedPieces is DroppedPieces + 1,
    NewScore is Score + AddedScore,
    NewGameState = [NewMatrix, NewScore, NewDroppedPieces, PieceSwap], !.

applyMove(GameState, 'Rotate', NewGameState) :-
    [Matrix, Score, DroppedPieces, PieceSwap] = GameState,
    rotateTetromino(Matrix, NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, PieceSwap], !.

applyMove(GameState, 'Swap', NewGameState) :-
    [Matrix, Score, DroppedPieces, PieceSwap] = GameState,
    swapTetromino(Matrix, PieceSwap, NewMatrix),
    getActiveTetromino(NewMatrix, NewSwapTetromino),
    NewGameState = [NewMatrix, Score, DroppedPieces, NewSwapTetromino], !.

applyMove(GameState, 'FullFall', NewGameState) :-
    [Matrix, Score, DroppedPieces, PieceSwap] = GameState,
    fullFall(Matrix, NewMatrix),
    goToNextCycle(NewMatrix, [AddedScore | NewMatrix]),
    NewDroppedPieces is DroppedPieces + 1,
    NewScore is Score + AddedScore,
    NewGameState = [NewMatrix, NewScore, NewDroppedPieces, PieceSwap], !.

applyMove(GameState, _, NewGameState) :-
    NewGameState = GameState.


%% pega o estado que o jogo ficará após o input
nextBoardState(GameState, Input, NewGameState) :-
    print('processing input: '), print(Input), nl,
    toLower(Input, InputLower),
    print('lower input: '), print(InputLower), nl,
    inputToMove(InputLower, Move),
    print('move: '), print(Move), nl,
    applyMove(GameState, Move, NewGameState).
    

% -- loop do jogo. descida autómatica das peças
% progressTime :: Float -> GameState -> GameState 

% -- define as pontuações de acordo com a quantidade de linhas limpas de uma só vez
% pointsForClear :: Int -> Int
pointsForClear(1, 100).
pointsForClear(2, 250).
pointsForClear(3, 500).
pointsForClear(4, 1000).
pointsForClear(_, 0).

% -- define um delay para que as peças caiam sozinhas baseado na quantidade de peças colocadas em jogo.
% droppedPiecesToDelay :: Int -> Float

%-- define as teclas para jogar
%inputToMove :: Event -> Move
inputToMove('a', 'Left').
inputToMove('d', 'Right').
inputToMove('s', 'Down').
inputToMove('w', 'Rotate').
inputToMove('c', 'Swap').
inputToMove('Space', 'FullFall').

main :-
    newGameState(GameState),
    [Matrix, _, _, _] = GameState,
    print('showing grid'), nl,
    showGrid(Matrix),
    gameLoop(GameState).

gameLoop(GameState) :-
    read(Input),
    print('input: '), print(Input), nl,
    nextBoardState(GameState, Input, NewGameState),
    print('showing grid'), nl,
    [Matrix, _, _, _] = NewGameState,
    showGrid(Matrix),
    gameLoop(NewGameState).
