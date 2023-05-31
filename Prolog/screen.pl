% -- showGrid, desenha o grid completo na tela
showGrid([]).
showGrid([ X | XS ]) :- 
    drawLine(X),
    showGrid(XS).

drawLine(RawLine) :-
    atom_chars(RawLine, CharsLst),
    convert(CharsLst, Line),
    writeln(Line).

convert([], "").
convert([ A | AS ], FinalLine) :-
    char_to_cell(A, ACell),
    convert(AS, Line),
    string_concat(ACell, Line, FinalLine).

% -- define as cores de cada quadrado
char_to_cell('r', "\033[91m██\033[91m"). % red (vermelho)
char_to_cell('g', "\033[92m██\033[92m"). % green (verde)
char_to_cell('y', "\033[93m██\033[93m"). % yellow (amarelo)
char_to_cell('b', "\033[94m██\033[94m"). % blue (azul)
char_to_cell('m', "\033[95m██\033[95m"). % magenta (roxo)
char_to_cell('c', "\033[96m██\033[96m"). % cyan (ciano)
char_to_cell('.', "\033[97m██\033[97m"). % white (branco)
   
% -- exibe a tela de game over com a pontuação atual
% showGameOver :: Int -> Picture
showGridLine([]).
showGridLine([H|T]) :-
    print(H),
    showGridLine(T).
showGrid([]).
showGrid([H|T]) :-
    showGrid(T),
    showGridLine(H), nl.
