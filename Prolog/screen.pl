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
    

numberToAscii(0, [Result]):- 
    Result is ['aaa', 'a a', 'a a', 'a a', 'aaa'].	    
numberToAscii(1, [Result]):- 
    Result is [' a ', 'aa ', ' a ', ' a ', 'aaa'].    
numberToAscii(2, [Result]):- 
    Result is ['aaa', '  a', 'aaa', 'a  ', 'aaa'].
numberToAscii(3, [Result]):- 
    Result is ['aaa', '  a', 'aa', '  a', 'aaa'].
numberToAscii(4, [Result]):-
    Result is ['a a', 'a a', 'aaa', '  a', '  a'].
numberToAscii(5, [Result]):-
    Result is ['aaa', 'a  ', 'aaa', '  a', 'aaa'].
numberToAscii(6, [Result]):-
    Result is ['aaa', 'a  ', 'aaa', 'a a', 'aaa'].
numberToAscii(7, [Result]):-
    Result is ['aaa', '  a', '  a', '  a', '  a'].
numberToAscii(8, [Result]):-
    Result is ['aaa', 'a a', 'aaa', 'a a', 'aaa'].
numberToAscii(9, [Result]):-
    Result is ['aaa', 'a a', 'aaa', '  a', '  a'].


getScoreDigits(0, []).

getScoreDigits(Score, [Result]):-
    NewScore is Score // 10,
    Module is Score mod 10,
    getScoreDigits(NewScore, [NewResult]),
    concatenate([NewResult], [Module], [Result]).




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

