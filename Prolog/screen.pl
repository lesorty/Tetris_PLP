% showGrid([]).
% showGrid([ X | XS ]) :- 
%     drawLine(X),
%     showGrid(XS).

% drawLine(CharsLst) :-
%     convert(CharsLst, Line),
%     writeln(Line).

% convert([], "").
% convert([ A | AS ], FinalLine) :-
%     toLower(A, ALower),
%     char_to_cell(ALower, ACell),
%     convert(AS, Line),
%     string_concat(ACell, Line, FinalLine).

% % -- define as cores de cada quadrado

% char_to_cell('a', "\033[91m██\033[91m"). % red (vermelho)
% char_to_cell('b', "\033[92m██\033[92m"). % green (verde)
% char_to_cell('c', "\033[93m██\033[93m"). % yellow (amarelo)
% char_to_cell('d', "\033[94m██\033[94m"). % blue (azul)
% char_to_cell('e', "\033[95m██\033[95m"). % magenta (roxo)
% char_to_cell('f', "\033[96m██\033[96m"). % cyan (ciano)
% char_to_cell('.', "\033[97m██\033[97m"). % white (branco)

showGridLine([]).
showGridLine([H|T]) :-
    write(H),
    showGridLine(T).
showGrid([]).
showGrid([H|T]) :-
    showGrid(T),
    showGridLine(H), nl.
    


