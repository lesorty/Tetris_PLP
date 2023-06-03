:- include('MatrixController.pl').

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