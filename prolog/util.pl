
:- module(util, [delimitar_cpf/2,
 limpar_terminal/0, 
 ler_json/2, 
 deletar_arquivo/1, 
 string_to_atom/2]).


:- usemodule(library(process)).
:- use_module(library(http/json)).

delimitar_cpf(CPF, CPFFormatado) :-
    filter_numbers(CPF, Numeros),
    length(Numeros, Length),
    (   Length =:= 11 ->
        dividir_numeros_cpf(Numeros, Chunk1, Chunk2, Chunk3, UltimosDois),
        maplist(number_to_atom, [Chunk1, Chunk2, Chunk3, UltimosDois], [AtomChunk1, AtomChunk2, AtomChunk3, AtomUltimosDois]),
        atomic_list_concat([AtomChunk1, '.', AtomChunk2, '.', AtomChunk3, '-', AtomUltimosDois], CPFFormatado)
    ;   CPFFormatado = 'CPF não possui 11 números'
    ).


dividir_numeros_cpf(Numeros, Chunk1, Chunk2, Chunk3, UltimosDois) :-
    length(Chunk1, 3),
    length(Chunk2, 3),
    length(Chunk3, 3),
    append([Chunk1, Chunk2, Chunk3, UltimosDois], Numeros).

filter_numbers(CPF, Numeros) :-
    include(number_char, CPF, Numeros).

number_to_atom(Number, Atom) :-
    atom_chars(Atom, Number).

number_char(Char) :-
    char_type(Char, digit).
    take(0, _, []).

take(0, _, []):-!.
take(N, [X|Xs], [X|Ys]) :-
    N > 0,
    M is N - 1,
    take(M, Xs, Ys).

ler_json(NomeArquivo, Conteudo) :-
    open(NomeArquivo, read, Stream),
    json_read_dict(Stream, Conteudo),
    close(Stream).

not_existe_file(Arquivo):-
    \+ exists_file(Arquivo).

deletar_arquivo(Arquivo) :-
    atom_concat('rm ', Arquivo, Command),
    shell(Command).

string_to_atom(String, Atom) :-
    string_chars(String, Chars),
    atom_chars(Atom, Chars).


limpar_terminal :-
    current_prolog_flag(windows, true),
    process_create(path(cmd), ['/C', 'cls'], [process(PID)]),
    process_wait(PID, ), !.

