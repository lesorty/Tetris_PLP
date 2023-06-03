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
:- include('Screen.pl').
:- include('ConfigHandler.pl').
:- include('MoveAppliers.pl').

newGameState(GameState) :-
    emptyMatrix(Matrix),
    getRandomTetromino(SwapTetromino),
    putRandomTetromino(Matrix, NewMatrix),
    get_time(Now),
    GameState = [NewMatrix, 0, 0, Now, SwapTetromino].

% -- aplica um movimento numa matriz
% applyMove :: GameState -> Move -> GameState

autoFall(Gamestate, TimeSinceLastMove, NewGameState) :-
    get_time(Now),
    DeltaTime is Now - TimeSinceLastMove,
    [_, _, DroppedPieces, _, _] = Gamestate,
    droppedPiecesToDelay(DroppedPieces, Delay),
    AutoFallCount is floor(DeltaTime / Delay),
    applyDownNTimes(Gamestate, AutoFallCount, GameStatePostAuto),
    [Matrix, Score, PostDroppedPieces, _, PieceSwap] = GameStatePostAuto,
    NewLastDropTime is Now - DeltaTime + (AutoFallCount * Delay),
    NewGameState = [Matrix, Score, PostDroppedPieces, NewLastDropTime, PieceSwap],
    !.

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
    getConfig('dificuldade', 'facil'),
    (DroppedPieces > 40 -> Delay = 0.6; Delay is 1 - (DroppedPieces / 100)),
    print('delay: '), print(Delay), nl.

droppedPiecesToDelay(DroppedPieces, Delay) :-
    getConfig('dificuldade', 'medio'),
    (DroppedPieces > 40 -> Delay = 0.4; Delay is 1 - (DroppedPieces / 66)),
    print('delay: '), print(Delay), nl.

droppedPiecesToDelay(DroppedPieces, Delay) :-
    getConfig('dificuldade', 'dificil'),
    (DroppedPieces > 40 -> Delay = 0.2; Delay is 1 - (DroppedPieces / 50)),
    print('delay: '), print(Delay), nl.

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
