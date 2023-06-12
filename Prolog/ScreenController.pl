:- include('MoveAppliers.pl').

% bota um espaço e deixa um caractere maiusculo
showChar(Char, Result) :-
    toUpper(Char, CharUpper),
    string_concat(' ', CharUpper, Result).

% retorna a representacao visual de uma linha de uma matriz
showGridLine([], '').
showGridLine([H|T], Result) :-
    showChar(H, Char),
    showGridLine(T, TailResult),
    string_concat(Char, TailResult, Result).

% retorna a representacao visual de uma matriz
showGrid([], []).
showGrid([H|T], [Result|TailResult]) :-
    showGridLine(H, Line),
    string_concat('|', Line, LineWithLeftBorder),
    string_concat(LineWithLeftBorder, '|', Result),
    showGrid(T, TailResult).

% retorna a representacao visual de uma matriz, com bordas
getVisualGrid(Grid, VisualGrid) :-
    showGrid(Grid, GridString),
    [Head | _] = GridString,
    string_length(Head, Length),
    repeatString('-', Length, Border),
    append([Border], GridString, GridStringWithTopBorder),
    append(GridStringWithTopBorder, [Border], VisualGrid).

% retorna o quadrado com o placar e o recorde
getVisualScore(Score, VisualScore) :-
    number_string(Score, ScoreString),
    padUntilLength(ScoreString, 10, PaddedScoreString), 
    string_concat(' | PLACAR: ', PaddedScoreString, CurrentScoreBorderless),
    string_concat(CurrentScoreBorderless, ' | ', CurrentScore),

    getConfig('highscore', HighScore),
    padUntilLength(HighScore, 9, PaddedHighScoreString),
    string_concat(' | RECORDE: ', PaddedHighScoreString, CurrentHighScoreBorderless),
    string_concat(CurrentHighScoreBorderless, ' | ', CurrentHighScore),

    VisualScore = [' ---------------------- ', CurrentScore, CurrentHighScore, ' ---------------------- '].

% retorna um textinho em ascii com o nome do jogo
getVisualTetris(VisualTetris) :-
    VisualTetris = ['  ___ __ ___ __    __   ',
                    '   |  |_  |  |_| | |_   ',
                    '   |  |_  |  |\\  |  _|  '].

% retorna o quadrado mostrando os controles
getVisualControls(VisualControls) :-
    VisualControls = [' ---------------------- ', 
                      ' | CONTROLES:         | ',
                      ' |                    | ',
                      ' |   W - RODA PECA    | ',
                      ' |   A - ESQUERDA     | ',
                      ' |   D - DIREITA      | ',
                      ' |   S - DESCE        | ',
                      ' |   V - QUEDA TOTAL  | ',
                      ' |   C - GUARDA PECA  | ',
                      ' |   X - SAIR         | ',
                      ' ---------------------- '].


% retorna o quadrado mostrando a proxima peca
getVisualNextTetromino(NextTetromino, VisualNextTetromino) :-
    getTetrominoBlocks(NextTetromino, Blocks),
    getTetrominoColor(NextTetromino, Color),
    addToAllLists(Blocks, [0, 1], RaisedBlocks),
    emptySmallMatrix(EmptyMatrix),
    updateMatrixElements(EmptyMatrix, Color, RaisedBlocks, NextTetrominoMatrix),
    getVisualGrid(NextTetrominoMatrix, UpsideDownVisualNextTetromino),
    reverse(UpsideDownVisualNextTetromino, NamelessVisualNextTetromino),
    [_ | Tail] = NamelessVisualNextTetromino,
    append(['---PROX---'], Tail, VisualNextTetromino).

% retorna o quadrado mostrando a peca guardada
getVisualSwapTetromino(SwapTetromino, VisualSwapTetromino) :-
    getTetrominoBlocks(SwapTetromino, Blocks),
    getTetrominoColor(SwapTetromino, Color),
    addToAllLists(Blocks, [0, 1], RaisedBlocks),
    emptySmallMatrix(EmptyMatrix),
    updateMatrixElements(EmptyMatrix, Color, RaisedBlocks, SwapTetrominoMatrix),
    getVisualGrid(SwapTetrominoMatrix, UpsideDownVisualSwapTetromino),
    reverse(UpsideDownVisualSwapTetromino, NamelessVisualSwapTetromino),
    [_ | Tail] = NamelessVisualSwapTetromino,
    append(['-GUARDADA-'], Tail, VisualSwapTetromino).

% mostra a tela baseada no estado do jogo
showGameState(GameState) :-
    [Matrix, Score, _, _, SwapTetromino, NextTetromino] = GameState,
    dropLast5Lines(Matrix, FrontendMatrix),
    getVisualGrid(FrontendMatrix, VisualGrid), 
    reverse(VisualGrid, AsciiGrid),


    getVisualScore(Score, VisualScore),
    emptyRectangle(2, 24, Rectangle1),
    getVisualTetris(VisualTetris),
    emptyRectangle(2, 24, Rectangle2),
    getVisualControls(VisualControls),


    append(VisualScore, Rectangle1, Partial1),
    append(Partial1, VisualTetris, Partial2),
    append(Partial2, Rectangle2, Partial3),
    append(Partial3, VisualControls, AsciiLeftSide),


    concatenateAsciiList([AsciiLeftSide, AsciiGrid], AsciiPartial1),
    emptyRectangle(21, 1, Col2Rectangle),
    concatenateAsciiList([AsciiPartial1, Col2Rectangle], AsciiPartial2),

    getVisualNextTetromino(NextTetromino, VisualNextTetromino),
    
    emptyRectangle(2, 10, BetweenNextSwapRectangle),
    getVisualSwapTetromino(SwapTetromino, VisualSwapTetromino),

    append(VisualNextTetromino, BetweenNextSwapRectangle, Partial4),
    append(Partial4, VisualSwapTetromino, Partial5),

    concatenateAsciiList([AsciiPartial2, Partial5], AsciiPartial3),

    nl,
    printAscii(AsciiPartial3).

% recebe um digito e retorna seu equivalente em representaçao ascii

repeatString(_, 0, '').
repeatString(String, Times, Result) :-
    Times > 0,
    NewTimes is Times - 1,
    repeatString(String, NewTimes, NewResult),
    string_concat(String, NewResult, Result).

% feito
% recebe uma lista de strings e retorna uma lista de strings com a representacao ascii de cada digito
concatenateAscii([], [], []).
concatenateAscii([H1|T1], [H2|T2], [Concatenated|Result]) :-
    concatenateAscii(T1, T2, Result),
    string_concat(H1, H2, Concatenated).

concatenateAscii([H1|T1], [], [Concatenated|Result]) :- 
    concatenateAscii(T1, [], Result),
    string_length(H1, Len),
    repeatString(' ', Len, Spaces),
    string_concat(H1, Spaces, Concatenated).

concatenateAscii([], [H2|T2], [Concatenated|Result]) :-
    concatenateAscii([], T2, Result),
    string_length(H2, Len),
    repeatString(' ', Len, Spaces),
    string_concat(Spaces, H2, Concatenated).

% recebe uma lista de elementos em ascii e retorna uma unica representação deles
concatenateAsciiList([], []).
concatenateAsciiList([Digit], Digit).
concatenateAsciiList([H|T], Result) :-
    concatenateAsciiList(T, TailResult),
    concatenateAscii(H, TailResult, Result).

% recebe uma lista de strings e imprime na tela
printAscii([]).
printAscii([H|T]) :-
    write(H), nl,
    printAscii(T).
    
% mostra a tela de gameover
showGameOver(Score, HighScore) :-

    number_string(Score, ScoreString),
    padUntilLength(ScoreString, 17, PaddedScoreString), 
    string_concat(' | PLACAR: ', PaddedScoreString, CurrentScoreBorderless),
    string_concat(CurrentScoreBorderless, ' | ', CurrentScoreString),

    padUntilLength(HighScore, 7, PaddedHighScoreString),
    string_concat(' | RECORDE ANTERIOR: ', PaddedHighScoreString, PrevHighScoreBorderless),
    string_concat(PrevHighScoreBorderless, ' | ', PrevHighScoreString),

    VisualScore = [' ---------GAME---OVER--------- ', CurrentScoreString, PrevHighScoreString, ' ----------------------------- '],
    (Score > HighScore -> append(VisualScore, [' | PARABENS!   NOVO RECORDE! |', ' ----------------------------- '], 
                                                NewVisualScore)
                                                ; 
                                                NewVisualScore = VisualScore),
    nl,nl,nl,nl,nl,nl,nl,nl,nl,
    printAscii(NewVisualScore),
    write('APERTE "x" PARA SAIR DO JOGO'), nl,
    nl,nl,nl,nl,nl,nl,nl,nl,nl,
    read(Option),
    (Option = 'x' -> halt; showGameOver(Score, HighScore)).

% bota espacos no comeco de uma string ate que ela tenha um tamanho especifico
padUntilLength(String, Length, Result) :-
    string_length(String, StringLength),
    Diff is Length - StringLength,
    repeatString(' ', Diff, Spaces),
    string_concat(Spaces, String, Result).

% retorna um retangulo vazio de tamanho Height x Width
emptyRectangle(0, _, []).
emptyRectangle(Height, Width, [H|T]) :-
    Height > 0,
    NewHeight is Height - 1,
    emptyRectangle(NewHeight, Width, T),
    repeatString(' ', Width, H).
