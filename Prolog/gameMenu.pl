:- include('main.pl').

main_menu :-
    show_menu(1),
    read(Input),
    selecao(Input, 1).

show_menu(1) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "p" para jogar'), nl,
    write('>>> Dificuldade Facil Medio Dificil'), nl,
    write('Volume'),
    getConfig('volume',Valor),
    converteNumVolumeBar(Valor, VolumeBar),
    write(' >'),
    write(VolumeBar),
    write('<'),
    nl,
    write('Exit'), nl,
    write('Enter your choice: '),
    ajustaBaixo.

show_menu(2) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "p" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('>>> Volume'),
    getConfig('volume',Valor),
    converteNumVolumeBar(Valor, VolumeBar),
    write(' >'),
    write(VolumeBar),
    write('<'),
    nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu(3) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "p" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('Volume'),
    getConfig('volume',Valor),
    converteNumVolumeBar(Valor, VolumeBar),
    write(' >'),
    write(VolumeBar),
    write('<'),
    nl,
    write('>>> Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

novo_input(Num) :-
    show_menu(Num),
    read(Input),
    selecao(Input, Num).

novo_input(0) :-
    show_menu(3),
    read(Input),
    selecao(Input, 3).

novo_input(4) :-
    show_menu(1),
    read(Input),
    selecao(Input, 1).

selecao('s', Num) :-
    NumTemp is Num + 1,
    novo_input(NumTemp),
    !.

selecao('p', _) :-
    main.

selecao('w', Num) :-
    NumTemp is Num - 1,
    novo_input(NumTemp),!.

selecao('d', 1) :-
    show_menu_levels(1),
    read(Input),
    selecao_levels(Input, 1), !.

selecao('d', 2) :-
    show_menu_volume(1),
    read(Input),
    selecao_volume(Input, 1), !.

selecao('j', Num) :-
    process_choice(Num),
    !.

selecao(_, Num) :-
    novo_input(Num).

process_choice(1) :-
    nl,
    show_menu_levels(1),
    read(Input),
    selecao_levels(Input, 1).


process_choice(2) :-
    write('Ajuste o volume...').

process_choice(3) :-
    write('Saindo...').





show_menu_levels(1) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('>>> Dificuldade [Facil] Medio Dificil'), nl,
    write('Volume'),
    getConfig('volume',Valor),
    converteNumVolumeBar(Valor, VolumeBar),
    write(' >'),
    write(VolumeBar),
    write('<'),
    nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu_levels(2) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('>>> Dificuldade Facil [Medio] Dificil'), nl,
    write('Volume'),
    getConfig('volume',Valor),
    converteNumVolumeBar(Valor, VolumeBar),
    write(' >'),
    write(VolumeBar),
    write('<'),
    nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu_levels(3) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('>>> Dificuldade Facil Medio [Dificil]'), nl,
    write('Volume'),
    getConfig('volume',Valor),
    converteNumVolumeBar(Valor, VolumeBar),
    write(' >'),
    write(VolumeBar),
    write('<'),
    nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.


novo_input_levels(Num) :-
    show_menu_levels(Num),
    read(Input),
    selecao_levels(Input, Num).

novo_input_levels(4) :-
    show_menu_levels(1),
    read(Input),
    selecao_levels(Input, 1).

novo_input_levels(0) :-
    show_menu_levels(3),
    read(Input),
    selecao_levels(Input, 3).

selecao_levels('d', Num) :-
    NumTemp is Num + 1,
    novo_input_levels(NumTemp), !.

selecao_levels('a', Num) :-
    NumTemp is Num - 1,
    novo_input_levels(NumTemp), !.

selecao_levels('k', Num) :-
    process_choice_levels(Num), !.

selecao_levels(_, Num) :-
    novo_input_levels(Num).

process_choice_levels(1) :-
    setConfig('dificuldade', 'facil'),
    novo_input(1).

process_choice_levels(2) :-
    setConfig('dificuldade', 'medio'),
    novo_input(1).

process_choice_levels(3) :-
    setConfig('dificuldade', 'dificil'),
    novo_input(1).


show_menu_volume(1) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('>>> Volume [     ]'), nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu_volume(2) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('>>> Volume [=    ]'), nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu_volume(3) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('>>> Volume [==   ]'), nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu_volume(4) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('>>> Volume [===  ]'), nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu_volume(5) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('>>> Volume [==== ]'), nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

show_menu_volume(6) :-
    ajustaCima,
    write('=== Game Menu ==='), nl,
    write('Pressione "P" para jogar'), nl,
    write('Dificuldade '), 
    getConfig('dificuldade', Value),
    write('>'),
    write(Value),
    write('<'),
    nl,
    write('>>> Volume [=====]'), nl,
    write('Exit'), nl,
    write('Enter your choice: '), ajustaBaixo.

selecao_volume('d', Num) :-
    NumTemp is Num + 1,
    novo_input_volume(NumTemp), !.

selecao_volume('a', Num) :-
    NumTemp is Num - 1,
    novo_input_volume(NumTemp), !.

selecao_volume('l', Num) :-
    process_choice_volume(Num), !.

selecao_volume(_, Num) :-
    novo_input_volume(Num).

novo_input_volume(Num) :-
    show_menu_volume(Num),
    read(Input),
    selecao_volume(Input, Num).

novo_input_volume(7) :-
    show_menu_volume(1),
    read(Input),
    selecao_volume(Input, 1).

novo_input_volume(0) :-
    show_menu_volume(6),
    read(Input),
    selecao_volume(Input, 6).

process_choice_volume(1) :-
    setConfig('volume', 0),
    novo_input(2).

process_choice_volume(2) :-
    setConfig('volume', 1),
    novo_input(2).

process_choice_volume(3) :-
    setConfig('volume', 2),
    novo_input(2).

process_choice_volume(4) :-
    setConfig('volume', 3),
    novo_input(2).

process_choice_volume(5) :-
    setConfig('volume', 4),
    novo_input(2).

process_choice_volume(6) :-
    setConfig('volume', 5),
    novo_input(2).

converteNumVolumeBar("0", '[     ]').
converteNumVolumeBar("1", '[=    ]').
converteNumVolumeBar("2", '[==   ]').
converteNumVolumeBar("3", '[===  ]').
converteNumVolumeBar("4", '[==== ]').
converteNumVolumeBar("5", '[=====]').

ajustaCima :-
nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl.

ajustaBaixo :-
nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl.