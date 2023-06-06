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

getConfig(KeyAtom, Value) :-
    var(Value),
    open('config.txt', read, Stream),
    atom_string(KeyAtom, Key),
    get_char(Stream, Char1),
    process_the_stream(Char1, Stream, Lines),
    close(Stream),
    linesIntoList(Lines, List),
    readConfig(Key, Value, List).

setConfig(KeyAtom, ValueAtom) :-
    write('setting '), write(KeyAtom), write(' '), write(ValueAtom), nl,
    atom_string(KeyAtom, Key),
    atom_string(ValueAtom, Value),
    open('config.txt', read, Stream),
    get_char(Stream, Char1),
    process_the_stream(Char1, Stream, Lines),
    close(Stream),
    linesIntoList(Lines, List),
    write('partial list '), print(List), nl,
    eraseInstances([Key, _], List, NewList),
    write('new list '), print(NewList), nl,
    append(NewList, [[Key, Value]], FinalList),
    write('final list '), print(FinalList), nl,
    open('config.txt', write, Stream2),
    writeConfig(Stream2, FinalList),
    close(Stream2).

writeConfig(_, []) :- !.
writeConfig(Stream, [[Key, Value]|T]) :-
    write('writing '), write(Key), write(' '), write(Value), nl,
    write(Stream, Key),
    write(Stream, "="),
    write(Stream, Value),
    write(Stream, ";"),
    writeConfig(Stream, T).

eraseInstances(_, [], []).
eraseInstances(X, [X|T], L) :- eraseInstances(X, T, L).
eraseInstances(X, [H|T], [H|L]) :- X \= H, eraseInstances(X, T, L).

linesIntoList(Lines, List) :-
    eraseInstances('\n', Lines, JoinedLines),
    atomic_list_concat(JoinedLines, '', JoinedString),
    split_string(JoinedString, ';', "", JoinedKeyValues),
    maplist(keyValueStringIntoList, JoinedKeyValues, ListPadded),
    eraseInstances([], ListPadded, List).

keyValueStringIntoList(JoinedKeyValue, [Key, Value]) :-
    split_string(JoinedKeyValue, "=", "", [KeyAtom, ValueAtom]),
    atom_string(KeyAtom, Key),
    atom_string(ValueAtom, Value).

keyValueStringIntoList(_, []).
    
readConfig(Key, Value, [[Key, Value]|_]) :- !.
readConfig(Key, Value, [[HKey, _]|T]) :-
    \+ Key = HKey,
    readConfig(Key, Value, T).

process_the_stream(end_of_file, _, []) :- !.

process_the_stream(Char, Stream, [Char|Lines]) :-
    get_char(Stream, Char2),
    process_the_stream(Char2, Stream, Lines).