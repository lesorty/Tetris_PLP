:- include('MatrixController.pl').
% aplica movimento
applyMove(GameState, 'Left', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino] = GameState,
    canMoveTetromino(Matrix, 'Left'),
    moveTetromino(Matrix, 'Left', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino], !.

applyMove(GameState, 'Left', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino] = GameState,
    \+(canMoveTetromino(Matrix, 'Left')),
    NewGameState = [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino], !.

applyMove(GameState, 'Right', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino] = GameState,
    canMoveTetromino(Matrix, 'Right'),
    moveTetromino(Matrix, 'Right', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino], !.

applyMove(GameState, 'Right', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino] = GameState,
    \+(canMoveTetromino(Matrix, 'Right')),
    NewGameState = [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap, NextTetromino] = GameState,
    get_time(Now),
    canMoveTetromino(Matrix, 'Down'),
    moveTetromino(Matrix, 'Down', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, Now, PieceSwap, NextTetromino], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap, NextTetromino] = GameState,
    get_time(Now),
    \+(canMoveTetromino(Matrix, 'Down')),
    goToNextCycle(Matrix, NextTetromino, [AddedScore | NewMatrix]),
    NewDroppedPieces is DroppedPieces + 1,
    NewScore is Score + AddedScore,
    getRandomTetromino(NewNextTetromino),
    NewGameState = [NewMatrix, NewScore, NewDroppedPieces, Now, PieceSwap, NewNextTetromino].

applyMove(GameState, 'Rotate', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino] = GameState,
    rotateTetromino(Matrix, NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino], !.

applyMove(GameState, 'Swap', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino] = GameState,
    swapTetromino(Matrix, PieceSwap, NewMatrix),
    getActiveTetromino(Matrix, NewSwapTetromino),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, NewSwapTetromino, NextTetromino], !.

applyMove(GameState, 'FullFall', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap, NextTetromino] = GameState,
    get_time(Now),
    fullFall(Matrix, NewMatrix),
    goToNextCycle(NewMatrix, NextTetromino, [AddedScore | PostCycleMatrix]),
    NewDroppedPieces is DroppedPieces + 1,
    NewScore is Score + AddedScore,
    getRandomTetromino(NewNextTetromino),
    NewGameState = [PostCycleMatrix, NewScore, NewDroppedPieces, Now, PieceSwap, NewNextTetromino], !.

applyMove(GameState, 'Invalid', NewGameState) :-
    NewGameState = GameState, !.

applyMove(GameState, 'UpdatePrediction', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino] = GameState,
    updatePrediction(Matrix, UpdatedMatrix),
    NewGameState = [UpdatedMatrix, Score, DroppedPieces, LastDropTime, PieceSwap, NextTetromino], !.

% aplica o movimento de descida n vezes
applyDownNTimes(GameState, _, GameState) :-
    [Matrix, _, _, _, _, _] = GameState,
    isGameOver(Matrix), !.


applyDownNTimes(GameState, 0, NewGameState) :-
    NewGameState = GameState, !.

applyDownNTimes(GameState, N, NewGameState) :-
    applyMove(GameState, 'Down', NewGameState1),
    NewN is N - 1,
    applyDownNTimes(NewGameState1, NewN, NewGameState), !.