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

% feito
% Recebe o Score e retorna a lista de digitos que o compoem
digitsToList(0, [0]).
digitsToList(Number, Digits) :-
    Number > 0,
    digitsToListHelper(Number, [], Digits).

digitsToListHelper(0, Acc, Acc).
digitsToListHelper(Number, Acc, Digits) :-
    Number > 0,
    Digit is Number mod 10,
    NewNumber is Number // 10,
    digitsToListHelper(NewNumber, [Digit|Acc], Digits).

%
% recebe um digito e retorna seu equivalente em representaçao ascii
numberToAscii(0, ['aaa  ', 'a a  ', 'a a  ', 'a a  ', 'aaa  ']).	    
numberToAscii(1, [' a   ', 'aa   ', ' a   ', ' a   ', 'aaa  ']).    
numberToAscii(2, ['aaa  ', '  a  ', 'aaa  ', 'a    ', 'aaa  ']).
numberToAscii(3, ['aaa  ', '  a  ', 'aa   ', '  a  ', 'aaa  ']).
numberToAscii(4, ['a a  ', 'a a  ', 'aaa  ', '  a  ', '  a  ']).
numberToAscii(5, ['aaa  ', 'a    ', 'aaa  ', '  a  ', 'aaa  ']).
numberToAscii(6, ['aaa  ', 'a    ', 'aaa  ', 'a a  ', 'aaa  ']).
numberToAscii(7, ['aaa  ', '  a  ', '  a  ', '  a  ', '  a  ']).
numberToAscii(8, ['aaa  ', 'a a  ', 'aaa  ', 'a a  ', 'aaa  ']).
numberToAscii(9, ['aaa  ', 'a a  ', 'aaa  ', '  a  ', '  a  ']).


% feito
% recebe uma lista de digitos e retorna uma lista de strings com a representacao ascii de cada digito
listToAsciiList([], []).
listToAsciiList([H|T], [NewResult | Result]):-
    numberToAscii(H, NewResult),
    listToAsciiList(T, Result).

% feito
% recebe uma lista de strings e retorna uma lista de strings com a representacao ascii de cada digito
concatenateAscii([], [], []).
concatenateAscii([H1|T1], [H2|T2], [Concatenated|Result]) :-
    concatenateAscii(T1, T2, Result),
    string_concat(H1, H2, Concatenated).

% TODO
% recebe uma lista de digitos em ascii e retorna uma unica representação deles
concatenateAsciiList([], []).
concatenateAsciiList([Digit|Digits], Result) :-
    concatenateAsciiList(Digits, Rest),
    concatenateAscii(Digit, Rest, Result).


% recebe um score e retorna sua representação em ascii
scoreToAscii(Score, Result) :-
    % feito
    digitsToList(Score, Digits),
    % feito
    listToAsciiList(Digits, AsciiDigits),

    concatenateAsciiList(AsciiDigits, Result).

% feito
% recebe uma lista de strings e imprime na tela
printAscii([]).
printAscii([H|T]) :-
    write(H), nl,
    printAscii(T).
    

showGameOver(Score) :-
    write('  a a a a a       a a a a a     a a         a a   a a a a a a a a'), nl,
    write('a a a a a a a   a a a a a a a   a a a     a a a   a a a a a a a a'), nl,
    write('a a       a a   a a       a a   a a a     a a a   a a '), nl,
    write('a a             a a       a a   a a a a a a a a   a a '), nl,
    write('a a   a a a     a a       a a   a a a a a a a a   a a a a a a'), nl,
    write('a a   a a a a   a a a a a a a   a a   a a   a a   a a a a a a'), nl,
    write('a a       a a   a a a a a a a   a a   a a   a a   a a '), nl,
    write('a a       a a   a a       a a   a a         a a   a a '), nl,
    write('a a       a a   a a       a a   a a         a a   a a '), nl,
    write('a a a a a a a   a a       a a   a a         a a   a a a a a a a a'), nl,
    write('    a a a a     a a       a a   a a         a a   a a a a a a a a'), nl,
    nl,
    write('  a a a a a     a a       a a   a a a a a a a a   a a a a a a a '), nl,
    write('a a a a a a a   a a       a a   a a a a a a a a   a a a a a a a a'), nl,
    write('a a       a a   a a       a a   a a               a a         a a'), nl,
    write('a a       a a   a a       a a   a a               a a         a a'), nl,
    write('a a       a a   a a       a a   a a a a a a       a a         a a'), nl,
    write('a a       a a   a a       a a   a a a a a a       a a a a a a a '), nl,
    write('a a       a a   a a       a a   a a               a a a   a a a '), nl,
    write('a a       a a   a a a   a a a   a a               a a         a a'), nl,
    write('a a       a a     a a a a a     a a               a a         a a'), nl,
    write('a a a a a a a         a a       a a a a a a a a   a a         a a'), nl,
    write('  a a a a a           a         a a a a a a a a   a a         a a'), nl,
    nl,
    

    write('Score: '), write(Score), nl.

