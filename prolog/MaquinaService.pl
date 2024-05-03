:- module(MaquinaService, [
    inicializar_arquivo_json_maquina/0,
    adicionar_maquina/2,
    criar_maquina/1,
    listar_maquinas/1,
    remover_maquina/1,
    atualizar_maquina_porCodigo/3,
    maquina_existe/1,
    adicionar_maquina_reparo/1,
    listar_maquinas_R/1,
    remover_maquina_manutencao/1,
    contar_maquinas/0,
    maquina_reparo/2,
    listar_datas/1,
    consultar_maquina/1
]).

:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(http/json_convert)).

inicializar_arquivo_json_maquina :-
    open('maquina.json', write, Stream),
    json_write(Stream, _{}),
    close(Stream).

maquina_existe(CodigoMaquina) :-
    atom_concat('BD/maquina/', CodigoMaquina, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

% Funcao para add maquina no arquivo
adicionar_maquina(NovaMaquina, MenuPrincipal) :-
    CodigoMaquina = NovaMaquina.codigoMaquina,
    (   maquina_existe(CodigoMaquina)
    ->  writeln("Máquina já existe!"),
        menu_maquina_g(MenuPrincipal)
    ;   atom_concat('BD/maquina/', CodigoMaquina, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, NovaMaquina),
        close(StreamWrite),
        menu_maquina_g(MenuPrincipal)
    ).

% Funcao para criar maquina
criar_maquina(MenuPrincipal) :-
    writeln("Digite o Codigo da Maquina: "),
    read_line_to_string(user_input, CodigoMaquina),
    (   maquina_existe(CodigoMaquina)
    ->  writeln("Maquina ja existe!"),
        menu_maquina_g(MenuPrincipal)
    ;   writeln("Nome da Maquina: "),
        read_line_to_string(user_input, NomeMaquina),
        writeln("Digite a data de manutencao (DDMMAAAA): "),
        read_line_to_string(user_input, DataManutencao),
        NovaMaquina = maquina{ codigoMaquina: CodigoMaquina, nomeMaquina: NomeMaquina, dataManutencao: DataManutencao },
        adicionar_maquina(NovaMaquina, MenuPrincipal),
        writeln("Maquina cadastrada com sucesso:"),
        writeln("Código da Maquina: " + CodigoMaquina),
        writeln("Nome da Maquina: " + NomeMaquina),
        writeln("Data de Manutencao: " + DataManutencao)
    ).

% Funcao para atualizar dados de uma maquina
atualizar_maquina_porCodigo(CodigoMaquina, NumeroCampo, NovoValor) :-
    (   maquina_existe(CodigoMaquina)
    ->  atom_concat('BD/maquina/', CodigoMaquina, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, Stream),
        json_read_dict(Stream, Maquina),
        close(Stream),

        (   NumeroCampo = 1 ->
                MaquinaAtualizada = Maquina.put(nomeMaquina, NovoValor)
            ;   NumeroCampo = 2 ->
                MaquinaAtualizada = Maquina.put(dataManutencao, NovoValor)
        ),

        writeln('\nAtualizando...'),
        sleep(2),

        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, MaquinaAtualizada),
        close(StreamWrite),

        writeln('\nMaquina Atualizada!'),
        sleep(2)
    ;   writeln("Maquina nao existe!")
    ).

% Função para remover uma maquina geral
remover_maquina(CodigoMaquina) :-
    (   maquina_existe(CodigoMaquina)
    ->  atom_concat('BD/maquina/', CodigoMaquina, Temp),
        atom_concat(Temp, '.json', Arquivo),
        delete_file(Arquivo),
        writeln("Maquina removida com sucesso!")
    ;   writeln("Maquina nao existe!")
    ).

% Função para remover uma maquina de manutenção
remover_maquina_manutencao(CodigoMaquina) :-
    (   maquina_existe(CodigoMaquina)
    ->  atom_concat('BD/reparo/', CodigoMaquina, Temp),
        atom_concat(Temp, '.json', Arquivo),
        delete_file(Arquivo),
        writeln("Maquina de manutencao removida com sucesso!")
    ;   writeln("Maquina de manutencao nao existe!")
    ).

% Funcao para listar todas as maquinas cadastradas
listar_maquinas(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json(Lista_Arquivos, Path).

processar_arquivos_json([], _).
processar_arquivos_json([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_maquina(Caminho),
        processar_arquivos_json(Arquivos, Diretorio)
    ;   processar_arquivos_json(Arquivos, Diretorio)
    ).

ler_e_mostrar_maquina(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Maquina),
    close(Stream),
    format("| Codigo: ~s~n", [Maquina.codigoMaquina]),
    format("| Nome: ~s~n", [Maquina.nomeMaquina]),
    format("| Data de Manutencao: ~s~n", [Maquina.dataManutencao]),
    writeln('').

% Funcao para maquinas com necessidade de reparo
adicionar_maquina_reparo(MenuPrincipal) :-
    writeln('Digite o ID da máquina que deseja adicionar ao reparo:'),
    read_line_to_string(user_input, CodigoMaquina),
    (   maquina_existe(CodigoMaquina)
    ->  atom_concat('BD/maquina/', CodigoMaquina, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, Maquina),
        close(StreamRead),
        atom_concat('BD/reparo/', CodigoMaquina, TempRep),
        atom_concat(TempRep, '.json', ArquivoRep),
        open(ArquivoRep, write, StreamWrite),
        json_write(StreamWrite, Maquina),
        close(StreamWrite),
        writeln('Máquina adicionada ao reparo.')
    ;   writeln('Máquina não existe!')
    ).

% Funcao para listar maquinas com necessidade de reparo
listar_maquinas_R(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json(Lista_Arquivos, Path).

processar_arquivos_json_R([], _).
processar_arquivos_json_R([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_maquina_R(Caminho),
        processar_arquivos_json_R(Arquivos, Diretorio)
    ;   processar_arquivos_json_R(Arquivos, Diretorio)
    ).

ler_e_mostrar_maquina_R(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Maquina),
    close(Stream),
    format("| Codigo: ~s~n", [Maquina.codigoMaquina]),
    format("| Nome: ~s~n", [Maquina.nomeMaquina]),
    format("| Data de Manutencao: ~s~n", [Maquina.dataManutencao]),
    writeln('').

:- use_module(library(system)).
% funcao que retorna a quantidade de maquinas cadastradas
contar_maquinas :-
    directory_files('BD/maquina', Files),
    exclude(system_file, Files, OnlyMachines),
    length(OnlyMachines, Quantidade),
    format('Existem ~w maquinas cadastradas.', [Quantidade]).

% Predicado auxiliar para excluir arquivos do sistema
system_file(File) :-
    sub_atom(File, 0, _, _, '.').

%Listar datas manutencao
listar_datas(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json_D(Lista_Arquivos, Path).

processar_arquivos_json_D([], _).
processar_arquivos_json_D([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_datas(Caminho),
        processar_arquivos_json_D(Arquivos, Diretorio)
    ;   processar_arquivos_json_D(Arquivos, Diretorio)
    ).

ler_e_mostrar_datas(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Maquina),
    close(Stream),
    format("| Nome: ~s~n", [Maquina.nomeMaquina]),
    format("| Data de Manutencao: ~s~n", [Maquina.dataManutencao]),
    writeln('').

% consulta uma maquina especifica
consultar_maquina(CodigoMaquina) :-
    (   maquina_existe(CodigoMaquina)
    ->  atom_concat('BD/maquina/', CodigoMaquina, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, Maquina),
        close(StreamRead),
        writeln("Informacoes da Maquina:"),
        format("| Codigo: ~s~n", [Maquina.codigoMaquina]),
        format("| Nome: ~s~n", [Maquina.nomeMaquina]),
        format("| Data de Manutencao: ~s~n", [Maquina.dataManutencao])
    ;   writeln("Maquina nao existe!")
    ).
