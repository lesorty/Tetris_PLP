:- include('GameController.pl').

% mostra o menu inicial do jogo
main :-
    show_menu(0),
    read_line_to_codes(user_input, InputCodes),
    string_to_atom(InputCodes, Input),
    selecao(Input, 0).

% mostra o menu quando a dificuldade está selecionada
show_menu(0) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    
    write('>>> '), showConfig('dificuldade'),
    showConfig('volume'), nl,
    write('Pressione "p" para jogar'), nl,
    write('Pressione "x" para sair'), nl,
    ajustaBaixo.

% mostra o menu quando o volume está selecionado
show_menu(1) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    showConfig('dificuldade'),
    write('>>> '), showConfig('volume'), nl,
    write('Pressione "p" para jogar'), nl,
    write('Pressione "x" para sair'), nl,
    ajustaBaixo.

%pega input e muda o menu
novo_input(Num) :-
    show_menu(Num),
    readInput(Input),
    selecao(Input, Num).

%seleciona a opção do menu
selecao('p', _) :-
    startGame.

selecao('s', Num) :-
    NumTemp is 1 - Num,
    novo_input(NumTemp),!.

selecao('w', Num) :-
    NumTemp is 1 - Num,
    novo_input(NumTemp),!.

selecao('d', 0) :-
    changeDiff('direita'),
    novo_input(0),!.

selecao('a', 0) :-
    changeDiff('esquerda'),
    novo_input(0),!.

selecao('d', 1) :-
    changeVolume('direita'),
    novo_input(1),!.

selecao('a', 1) :-
    changeVolume('esquerda'),
    novo_input(1),!.

selecao('x', _) :-
    halt.

selecao(_, Num) :-
    novo_input(Num).

%converte o numero do volume para a barra visual de volume
converteNumVolumeBar("0", '[     ]').
converteNumVolumeBar("1", '[=    ]').
converteNumVolumeBar("2", '[==   ]').
converteNumVolumeBar("3", '[===  ]').
converteNumVolumeBar("4", '[==== ]').
converteNumVolumeBar("5", '[=====]').

%modifica o volume
changeVolume('direita') :-
    getConfig('volume', Value),
    ValueTemp is (Value + 1) mod 6,
    setConfig('volume', ValueTemp).

changeVolume('esquerda') :-
    getConfig('volume', Value),
    ValueTemp is (Value - 1) mod 6,
    setConfig('volume', ValueTemp).

%converte o index da dificuldade para o nome da dificuldade
indexToDiff(0, "facil").
indexToDiff(1, "medio").
indexToDiff(2, "dificil").

%modifica a dificuldade
changeDiff('direita') :-
    getConfig('dificuldade', Value),
    indexToDiff(Index, Value),
    IndexTemp is (Index + 1) mod 3,
    indexToDiff(IndexTemp, ValueTemp),
    setConfig('dificuldade', ValueTemp).

changeDiff('esquerda') :-
    getConfig('dificuldade', Value),
    indexToDiff(Index, Value),
    IndexTemp is (Index - 1) mod 3,
    indexToDiff(IndexTemp, ValueTemp),
    setConfig('dificuldade', ValueTemp).

%mostra a configuração atual
showConfig('volume') :-
    getConfig('volume', Value),
    converteNumVolumeBar(Value, Bar),
    write('Volume >'), write(Bar), write('<'), nl.

showConfig('dificuldade') :-
    getConfig('dificuldade', 'facil'),
    write('Dificuldade [Facil] Medio Dificil'), nl.

showConfig('dificuldade') :-
    getConfig('dificuldade', 'medio'),
    write('Dificuldade Facil [Medio] Dificil'), nl.

showConfig('dificuldade') :-
    getConfig('dificuldade', 'dificil'),
    write('Dificuldade Facil Medio [Dificil]'), nl.


ajustaCima :-
nl,nl,nl,nl,nl,nl,nl,nl.

ajustaBaixo :-
nl,nl,nl,nl,nl,nl,nl,nl,nl.