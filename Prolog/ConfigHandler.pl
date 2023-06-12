% Retorna o valor de uma chave do arquivo de configuração
getConfig(KeyAtom, ValueAtom) :-
    \+ var(ValueAtom),
    open('config.txt', read, Stream),
    atom_string(KeyAtom, Key),
    atom_string(ValueAtom, Value),
    get_char(Stream, Char1),
    process_the_stream(Char1, Stream, Lines),
    close(Stream),
    linesIntoList(Lines, List),
    readConfig(Key, Value, List).

% Retorna o valor de uma chave do arquivo de configuração
getConfig(KeyAtom, Value) :-
    var(Value),
    open('config.txt', read, Stream),
    atom_string(KeyAtom, Key),
    get_char(Stream, Char1),
    process_the_stream(Char1, Stream, Lines),
    close(Stream),
    linesIntoList(Lines, List),
    readConfig(Key, Value, List).

% Seta o valor de uma chave do arquivo de configuração
setConfig(KeyAtom, ValueAtom) :-
    write('setting config '), write(KeyAtom), write(' to '), write(ValueAtom), nl,
    atom_string(KeyAtom, Key),
    atom_string(ValueAtom, Value),
    open('config.txt', read, Stream),
    get_char(Stream, Char1),
    process_the_stream(Char1, Stream, Lines),
    close(Stream),
    linesIntoList(Lines, List),
    eraseInstances([Key, _], List, NewList),
    append(NewList, [[Key, Value]], FinalList),
    open('config.txt', write, Stream2),
    writeConfig(Stream2, FinalList),
    close(Stream2).

% Escreve uma lista de chave-valor no arquivo de configuração
writeConfig(_, []) :- !.
writeConfig(Stream, [[Key, Value]|T]) :-
    write(Stream, Key),
    write(Stream, "="),
    write(Stream, Value),
    write(Stream, ";"),
    writeConfig(Stream, T).

% Apaga todas as instâncias de um elemento de uma lista
eraseInstances(_, [], []).
eraseInstances(X, [X|T], L) :- eraseInstances(X, T, L).
eraseInstances(X, [H|T], [H|L]) :- X \= H, eraseInstances(X, T, L).

% Transforma uma lista de strings em uma lista de pareamentos chave-valor
linesIntoList(Lines, List) :-
    eraseInstances('\n', Lines, JoinedLines),
    atomic_list_concat(JoinedLines, '', JoinedString),
    split_string(JoinedString, ';', "", JoinedKeyValues),
    maplist(keyValueStringIntoList, JoinedKeyValues, ListPadded),
    eraseInstances([], ListPadded, List).

% Transforma uma string chave-valor em uma lista de 2 elementos: chave e valor
keyValueStringIntoList(JoinedKeyValue, [Key, Value]) :-
    split_string(JoinedKeyValue, "=", "", [KeyAtom, ValueAtom]),
    atom_string(KeyAtom, Key),
    atom_string(ValueAtom, Value).

keyValueStringIntoList(_, []).
    
% Retorna o valor de uma chave em uma lista de chave-valor
readConfig(Key, Value, [[Key, Value]|_]) :- !.
readConfig(Key, Value, [[HKey, _]|T]) :-
    \+ Key = HKey,
    readConfig(Key, Value, T).

% Processa a stream de entrad
process_the_stream(end_of_file, _, []) :- !.

process_the_stream(Char, Stream, [Char|Lines]) :-
    get_char(Stream, Char2),
    process_the_stream(Char2, Stream, Lines).