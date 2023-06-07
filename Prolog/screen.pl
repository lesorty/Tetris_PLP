% Screen

% -- showGrid, desenha o grid completo na tela
% showGrid :: [[Square]] -> Int -> Picture

% -- Define a largura e a altura de cada célula do grid
% cellWidth :: Float

% -- a janela do jogo
% window :: Display

% -- define as cores de cada quadrado
% colorForSquare :: Square -> Color

% -- converte uma coordenada de célula (x, y) em uma posição da tela (x, y)
% cellToScreen :: (Int, Int) -> (Float, Float)

% -- desenha uma célula na tela, com divisórias
% drawCell :: (Int, Int) -> Square -> Picture
   
% -- exibe a tela de game over com a pontuação atual
% showGameOver :: Int -> Picture
showGridLine([]).
showGridLine([H|T]) :-
    write(H),
    showGridLine(T).
showGrid([]).
showGrid([H|T]) :-
    showGrid(T),
    showGridLine(H), nl.

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
    
% recebe um digito e retorna seu equivalente em representaçao ascii
-a--aaa-
aa----a-
-a--aaa-
-a--a---
aaa-aaa-
numberToAscii(0, [Result]):- 
    Result is ['aaa ', 'a a ', 'a a ', 'a a ', 'aaa '].	    
numberToAscii(1, [Result]):- 
    Result is [' a  ', 'aa  ', ' a  ', ' a  ', 'aaa '].    
numberToAscii(2, [Result]):- 
    Result is ['aaa ', '  a ', 'aaa ', 'a   ', 'aaa '].
numberToAscii(3, [Result]):- 
    Result is ['aaa ', '  a ', 'aa ', '  a ', 'aaa '].
numberToAscii(4, [Result]):-
    Result is ['a a ', 'a a ', 'aaa ', '  a ', '  a '].
numberToAscii(5, [Result]):-
    Result is ['aaa ', 'a   ', 'aaa ', '  a ', 'aaa '].
numberToAscii(6, [Result]):-
    Result is ['aaa ', 'a   ', 'aaa ', 'a a ', 'aaa '].
numberToAscii(7, [Result]):-
    Result is ['aaa ', '  a ', '  a ', '  a ', '  a '].
numberToAscii(8, [Result]):-
    Result is ['aaa ', 'a a ', 'aaa ', 'a a ', 'aaa '].
numberToAscii(9, [Result]):-
    Result is ['aaa ', 'a a ', 'aaa ', '  a ', '  a '].


% recebe uma lista de digitos e retorna uma lista de strings com a representacao ascii de cada digito
listToAsciiList([], []).
listToAsciiList([H|T], [NewResult | Result]):-
    numberToAscii(H, NewResult),
    listToAsciiList(T, Result).

% recebe uma lista de strings e retorna uma lista de strings com a representacao ascii de cada digito
concatenateAscii([], [], []).
concatenateAscii([H1|T1], [H2|T2], [Concatenated|Result]) :-
    concatenateAscii(T1, T2, Result),
    string_concat(H1, H2, Concatenated).

% recebe uma lista de digitos em ascii e retorna uma unica representação deles
concatenateAsciiList([], []).
concatenateAsciiList([Digit | Rest], Result) :-
    concatenateAsciiList(Rest, RestResult),
    concatenateAscii(Digit, RestResult, Result).

% recebe um score e retorna sua representação em ascii
scoreToAscii(Score, Result) :-
    digitsToList(Score, Digits),
    listToAsciiList(Digits, AsciiDigits),
    concatenateAsciiList(AsciiDigits, Result).

    



  a a a a a       a a a a a     a a         a a   a a a a a a a a
a a a a a a a   a a a a a a a   a a a     a a a   a a a a a a a a
a a       a a   a a       a a   a a a     a a a   a a 
a a             a a       a a   a a a a a a a a   a a 
a a   a a a     a a       a a   a a a a a a a a   a a a a a a
a a   a a a a   a a a a a a a   a a   a a   a a   a a a a a a
a a       a a   a a a a a a a   a a   a a   a a   a a 
a a       a a   a a       a a   a a         a a   a a
a a       a a   a a       a a   a a         a a   a a 
a a a a a a a   a a       a a   a a         a a   a a a a a a a a
    a a a a     a a       a a   a a         a a   a a a a a a a a

  a a a a a     a a       a a   a a a a a a a a   a a a a a a a 
a a a a a a a   a a       a a   a a a a a a a a   a a a a a a a a
a a       a a   a a       a a   a a               a a         a a
a a       a a   a a       a a   a a               a a         a a
a a       a a   a a       a a   a a a a a a       a a         a a
a a       a a   a a       a a   a a a a a a       a a a a a a a 
a a       a a   a a       a a   a a               a a a   a a a 
a a       a a   a a a   a a a   a a               a a         a a
a a       a a     a a a a a     a a               a a         a a
a a a a a a a         a a       a a a a a a a a   a a         a a
  a a a a a           a         a a a a a a a a   a a         a a
*/

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

