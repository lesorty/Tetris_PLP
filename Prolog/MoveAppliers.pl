:- include('MatrixController.pl').

applyMove(GameState, 'Left', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    canMoveTetromino(Matrix, 'Left'),
    moveTetromino(Matrix, 'Left', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Left', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    \+(canMoveTetromino(Matrix, 'Left')),
    NewGameState = [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Right', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    canMoveTetromino(Matrix, 'Right'),
    moveTetromino(Matrix, 'Right', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Right', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    \+(canMoveTetromino(Matrix, 'Right')),
    NewGameState = [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap] = GameState,
    get_time(Now),
    canMoveTetromino(Matrix, 'Down'),
    moveTetromino(Matrix, 'Down', NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, Now, PieceSwap], !.

applyMove(GameState, 'Down', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap] = GameState,
    get_time(Now),
    \+(canMoveTetromino(Matrix, 'Down')),
    goToNextCycle(Matrix, [AddedScore | NewMatrix]),
    NewDroppedPieces is DroppedPieces + 1,
    NewScore is Score + AddedScore,
    NewGameState = [NewMatrix, NewScore, NewDroppedPieces, Now, PieceSwap].

applyMove(GameState, 'Rotate', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    rotateTetromino(Matrix, NewMatrix),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.

applyMove(GameState, 'Swap', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    swapTetromino(Matrix, PieceSwap, NewMatrix),
    getActiveTetromino(Matrix, NewSwapTetromino),
    NewGameState = [NewMatrix, Score, DroppedPieces, LastDropTime, NewSwapTetromino], !.

applyMove(GameState, 'FullFall', NewGameState) :-
    [Matrix, Score, DroppedPieces, _, PieceSwap] = GameState,
    get_time(Now),
    fullFall(Matrix, NewMatrix),
    goToNextCycle(NewMatrix, [AddedScore | PostCycleMatrix]),
    NewDroppedPieces is DroppedPieces + 1,
    NewScore is Score + AddedScore,
    NewGameState = [PostCycleMatrix, NewScore, NewDroppedPieces, Now, PieceSwap], !.

applyMove(GameState, 'Invalid', NewGameState) :-
    NewGameState = GameState, !.

applyMove(GameState, 'UpdatePrediction', NewGameState) :-
    [Matrix, Score, DroppedPieces, LastDropTime, PieceSwap] = GameState,
    updatePrediction(Matrix, UpdatedMatrix),
    NewGameState = [UpdatedMatrix, Score, DroppedPieces, LastDropTime, PieceSwap], !.


applyDownNTimes(GameState, 0, NewGameState) :-
    NewGameState = GameState, !.

applyDownNTimes(GameState, N, NewGameState) :-
    applyMove(GameState, 'Down', NewGameState1),
    NewN is N - 1,
    applyDownNTimes(NewGameState1, NewN, NewGameState), !.