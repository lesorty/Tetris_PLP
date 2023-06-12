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
:- include('ScreenController.pl').
:- include('ConfigHandler.pl').

% define o estado inicial do jogo
newGameState(GameState) :-
    emptyMatrix(Matrix),
    getRandomTetromino(SwapTetromino),
    getRandomTetromino(FirstTetromino),
    putNextTetromino(Matrix, FirstTetromino, NewMatrix),
    getRandomTetromino(NextTetromino),
    get_time(Now),
    PredictionlessGameState = [NewMatrix, 0, 0, Now, SwapTetromino, NextTetromino],
    applyMove(PredictionlessGameState, 'UpdatePrediction', GameState).

% causa quedas automáticas baseadas no tempo desde o último movimento
autoFall(Gamestate, TimeSinceLastMove, NewGameState) :-
    get_time(Now),
    DeltaTime is Now - TimeSinceLastMove,
    [_, _, DroppedPieces, _, _, _] = Gamestate,
    droppedPiecesToDelay(DroppedPieces, Delay),
    AutoFallCount is floor(DeltaTime / Delay),
    applyDownNTimes(Gamestate, AutoFallCount, GameStatePostAuto),
    [Matrix, Score, PostDroppedPieces, _, PieceSwap, NextTetromino] = GameStatePostAuto,
    NewLastDropTime is Now - DeltaTime + (AutoFallCount * Delay),
    NewGameState = [Matrix, Score, PostDroppedPieces, NewLastDropTime, PieceSwap, NextTetromino],
    !.

% pega o estado que o jogo ficará após o input
nextBoardState(GameState, Input, NewGameState) :-
    toLower(Input, InputLower),
    inputToMove(InputLower, Move),
    [_, _, _, LastDropTime, _, _] = GameState,	
    autoFall(GameState, LastDropTime, AutoFallAppliedGameState),
    applyMove(AutoFallAppliedGameState, Move, PredictionlessGameState),
    applyMove(PredictionlessGameState, 'UpdatePrediction', NewGameState).    

 

% -- define as pontuações de acordo com a quantidade de linhas limpas de uma só vez
pointsForClear(1, 100).
pointsForClear(2, 250).
pointsForClear(3, 500).
pointsForClear(4, 1000).
pointsForClear(_, 0).

%define as teclas para jogar
inputToMove('a', 'Left') :- !.
inputToMove('d', 'Right') :- !.
inputToMove('s', 'Down'):- !.
inputToMove('w', 'Rotate'):- !.
inputToMove('c', 'Swap'):- !.
inputToMove('v', 'FullFall'):- !.
inputToMove(_, 'Invalid').

% define o tempo entre quedas automáticas baseado na dificuldade e na quantidade de peças já caídas
droppedPiecesToDelay(DroppedPieces, Delay) :-
    getConfig('dificuldade', 'facil'),
    (DroppedPieces > 40 -> Delay = 0.6; Delay is 1 - (DroppedPieces / 100)).

droppedPiecesToDelay(DroppedPieces, Delay) :-
    getConfig('dificuldade', 'medio'),
    (DroppedPieces > 40 -> Delay = 0.4; Delay is 1 - (DroppedPieces / 66)).

droppedPiecesToDelay(DroppedPieces, Delay) :-
    getConfig('dificuldade', 'dificil'),
    (DroppedPieces > 40 -> Delay = 0.2; Delay is 1 - (DroppedPieces / 50)).

% inicia jogo
startGame :-
    newGameState(GameState),
    showGameState(GameState),
    gameLoop(GameState).

% loop do jogo
gameLoop(GameState) :-
    readInput(Input),
    (Input == 'x' -> 
        !
        ;
        nextBoardState(GameState, Input, NewGameState),
        [Matrix, Score, _, _, _, _] = NewGameState,
        (isGameOver(Matrix) -> 
           getConfig('highscore', HighscoreStr), 
           atom_number(HighscoreStr, Highscore),
            (Score > Highscore -> 
                setConfig('highscore', Score), !
                ;
                true, !
            ),
            showGameOver(Score, Highscore), !
            ;
            showGameState(NewGameState), gameLoop(NewGameState))).
