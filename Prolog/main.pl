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
    get_time(Now),
    GameState = [NewMatrix, 0, 0, Now, SwapTetromino].

% -- aplica um movimento numa matriz
% applyMove :: GameState -> Move -> GameState
applyMove(GameState, 'Left', NewGameState) :-
    print('left move'), nl,
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    canMoveTetromino(Matrix, 'Left'),
    print('can move left'), nl,
    moveTetromino(Matrix, 'Left', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Left', NewGameState) :-
    print('left move'), nl,
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    \+(canMoveTetromino(Matrix, 'Left')),
    NewGameState = [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Right', NewGameState) :-
    print('right move'), nl,
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    canMoveTetromino(Matrix, 'Right'),
    moveTetromino(Matrix, 'Right', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Right', NewGameState) :-
    print('right move'), nl,
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    \+(canMoveTetromino(Matrix, 'Right')),
    NewGameState = [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap] = GameState,
    get_time(Now),
    canMoveTetromino(Matrix, 'Down'),
    print('down move'), nl,
    moveTetromino(Matrix, 'Down', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, Now, PieceSwap], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap] = GameState,
    get_time(Now),
    \+(canMoveTetromino(Matrix, 'Down')),
    print('down move but cant'), nl,
    goToNextCycle(Matrix, [AddedScore | NewMatrix]),
    print('next cycle'), nl,
    NewDroppedPieces is DroppedPieces + 1,
    NewScore is Score + AddedScore,
    NewGameState = [NewMatrix, NewScore, NewDroppedPieces, Now, PieceSwap],
    print('applied loop'), nl.

applyMove(GameState, 'Rotate', NewGameState) :-
    print('rotate move'), nl,
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    rotateTetromino(Matrix, NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Swap', NewGameState) :-
    print('swap move'), nl,
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    swapTetromino(Matrix, PieceSwap, NewMatrix),
    write('swap tetromino'),nl,
    getActiveTetromino(Matrix, NewSwapTetromino),
    write('get active tetromino'),nl,
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, NewSwapTetromino], !.

applyMove(GameState, 'FullFall', NewGameState) :-
    print('full fall move'), nl,
    [Matrix, Score, DroppedPieces, _, PieceSwap] = GameState,
    get_time(Now),
    fullFall(Matrix, NewMatrix),
    print('managed to fullfall'), nl,
    goToNextCycle(NewMatrix, [AddedScore | PostCycleMatrix]),
    print('next cycle'), nl,
    NewDroppedPieces is DroppedPieces + 1,
    print('dropped pieces: '), print(NewDroppedPieces), nl,
    NewScore is Score + AddedScore,
    print('score: '), print(NewScore), nl,
    NewGameState = [PostCycleMatrix, NewScore, NewDroppedPieces, Now, PieceSwap], !.

applyMove(GameState, 'Invalid', NewGameState) :-
    print('invalid move'), nl,
    NewGameState = GameState, !.

applyDownNTimes(GameState, 0, NewGameState) :-
    NewGameState = GameState, !.

applyDownNTimes(GameState, N, NewGameState) :-
    applyMove(GameState, 'Down', NewGameState1),
    NewN is N - 1,
    applyDownNTimes(NewGameState1, NewN, NewGameState), !.




autoFall(Gamestate, TimeSinceLastMove, NewGameState) :-
    get_time(Now),
    DeltaTime is Now - TimeSinceLastMove,
    [_, _, DroppedPieces, _, _] = Gamestate,
    droppedPiecesToDelay(DroppedPieces, Delay),
    AutoFallCount is floor(DeltaTime / Delay),
    print('delta time: '), print(DeltaTime), print(' auto fall count: '), print(AutoFallCount), nl,
    applyDownNTimes(Gamestate, AutoFallCount, GameStatePostAuto),
    print('applied down n times'), nl,
    [Matrix, Score, PostDroppedPieces, _, PieceSwap] = GameStatePostAuto,
    print('defined matrix, score, dropped pieces, piece swap'), nl,
    NewLastDropTime is Now - DeltaTime + (AutoFallCount * Delay),
    NewGameState = [Matrix, Score, PostDroppedPieces, NewLastDropTime, PieceSwap],
    print('defined newgamestate'), !.

%% pega o estado que o jogo ficará após o input
nextBoardState(GameState, Input, NewGameState) :-
    toLower(Input, InputLower),
    inputToMove(InputLower, Move),
    print('move: '), print(Move), nl, 
    [_, _, _, LastDropTime, _] = GameState,	
    print('last drop time: '), print(LastDropTime), nl,
    autoFall(GameState, LastDropTime, AutoFallAppliedGameState),
    print('autofell'), nl,
    applyMove(AutoFallAppliedGameState, Move, NewGameState).
    

% -- loop do jogo. descida autómatica das peças
% progressTime :: Float -> GameState -> GameState 

% -- define as pontuações de acordo com a quantidade de linhas limpas de uma só vez
% pointsForClear :: Int -> Int
pointsForClear(1, 100).
pointsForClear(2, 250).
pointsForClear(3, 500).
pointsForClear(4, 1000).
pointsForClear(_, 0).

%-- define as teclas para jogar
%inputToMove :: Event -> Move
inputToMove('a', 'Left') :- !.
inputToMove('d', 'Right') :- !.
inputToMove('s', 'Down'):- !.
inputToMove('w', 'Rotate'):- !.
inputToMove('c', 'Swap'):- !.
inputToMove('v', 'FullFall'):- !.
inputToMove(_, 'Invalid').


droppedPiecesToDelay(DroppedPieces, Delay) :-
    getConfig('dificuldade', 'medio'),
    (DroppedPieces > 35 -> Delay = 0.5; Delay is 1 - (DroppedPieces / 50)),
    print('delay: '), print(Delay), nl.

getConfig('dificuldade', 'medio').

main :-
    print('starting game'), nl,
    newGameState(GameState),
    [Matrix, _, _, _, _] = GameState,
    showGrid(Matrix),
    gameLoop(GameState).

gameLoop(GameState) :-
    read(Input),
    print('input: '), print(Input), nl,
    (Input == 'x' -> 
        print('exiting'), !
        ;
        nextBoardState(GameState, Input, NewGameState),
        [Matrix, Score, _, _, _] = NewGameState,
        (isGameOver(Matrix) -> 
            print('game over'), nl, !
            ;
            write('score is '), write(Score), nl, 
            showGrid(Matrix), gameLoop(NewGameState))).
