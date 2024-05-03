:- module(aulaService, [listar_todas_aulas/1, aula_existe/1]).


:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).

aula_existe(Nome):-
    atom_concat('BD/aula/', Nome, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

imprimir_planos([]).
imprimir_planos([Plano|Resto]) :-
    write(Plano),
    imprimir_planos_aux(Resto).

imprimir_planos_aux([]).
imprimir_planos_aux([Plano|Resto]):-
    write(','), write(Plano),
    imprimir_planos_aux(Resto).

listar_todas_aulas(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json(Lista_Arquivos, Path).

processar_arquivos_json([], _).
processar_arquivos_json([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_aula(Caminho),
        processar_arquivos_json(Arquivos, Diretorio)
    ;   processar_arquivos_json(Arquivos, Diretorio)
    ).

ler_e_mostrar_aula(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Aula),
    close(Stream),
    format('=>~s~n', [Aula.nome]),
    format('  Horario: ~s~n', [Aula.horario]),
    write('  Valida para os planos: '),
    imprimir_planos(Aula.planos),
    writeln('\n').
