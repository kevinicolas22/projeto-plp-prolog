:- module(AvaliacaoService, [
    inicializar_arquivo_json/0,
    adicionar_avaliacao_fisica/2,
    criar_avaliacao_fisica/1,
    menu_funcionario_avaliacao/1,
    ler_avaliacao_fisica_por_cpf/1,
    listar_todas_avaliacoes_fisicas/1,
    remover_avaliacao_fisica_por_cpf/1,
    atualizarAvaliacaoPorCPF/3
]).

:- use_module(util).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).

:- use_module(mainFuncionario, [menu_avaliacao_fisica/1]).


% Verifica se uma avaliacao fisica ja existe
avaliacao_fisica_existe(CPF) :-
    atom_concat('BD/avaliacao_fisica/', CPF, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

adicionar_avaliacao_fisica(NovaAvaliacaoFisica, MenuPrincipal) :-
    CPF = NovaAvaliacaoFisica.cpf,
    (   avaliacao_fisica_existe(CPF)
    ->  writeln("Avaliaco fisica ja existe!"),
        menu_avaliacao_fisica(MenuPrincipal)
    ;   atom_concat('BD/avaliacao_fisica/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, NovaAvaliacaoFisica),
        close(StreamWrite),
        menu_avaliacao_fisica(MenuPrincipal)
    ).

criar_avaliacao_fisica(MenuPrincipal) :-
    writeln("Digite o CPF do aluno para avaliacao fisica: "),
    read_line_to_string(user_input, CPF),
    (   avaliacao_fisica_existe(CPF)
    ->  writeln("Avaliacao fisica ja existe!"),
        menu_avaliacao_fisica(MenuPrincipal)
    ;   writeln("Data da Avaliacao (DDMMAAAA): "),
        read_line_to_string(user_input, DataAvaliacao),
        writeln("Peso (kg): "),
        read_line_to_string(user_input, Peso),
        writeln("Altura (m): "),
        read_line_to_string(user_input, Altura),
        writeln("Idade: "),
        read_line_to_string(user_input, Idade),
        writeln("Objetivo: "),
        read_line_to_string(user_input, Objetivo),
        NovaAvaliacaoFisica = avaliacao_fisica{
            cpf : CPF,
            dataAvaliacao: DataAvaliacao,
            peso: Peso,
            altura: Altura,
            idade: Idade,
            objetivo: Objetivo
        },
        adicionar_avaliacao_fisica(NovaAvaliacaoFisica, MenuPrincipal),
        writeln("Avaliacao fisica cadastrada com sucesso:"),
        writeln("CPF: " + Cpf),
        writeln("Data da Avaliacao: " + DataAvaliacao),
        writeln("Peso: " + Peso + " kg"),
        writeln("Altura: " + Altura + " m"),
        writeln("Idade: " + Idade),
        writeln("Objetivo: " + Objetivo)
    ).

ler_avaliacao_fisica_por_cpf(CPF) :-
    (   avaliacao_fisica_existe(CPF)
    ->  atom_concat('BD/avaliacao_fisica/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, AvaliacaoFisica),
        close(StreamRead),
        writeln("Informacoes da Avaliacao Fisica:"),
        format("CPF: ~s~n", [AvaliacaoFisica.cpf]),
        format("Data da Avaliacao: ~s~n", [AvaliacaoFisica.dataAvaliacao]),
        format("Peso: ~s~n", [AvaliacaoFisica.peso]),
        format("Altura: ~s~n", [AvaliacaoFisica.altura]),
        format("Idade: ~s~n", [AvaliacaoFisica.idade]),
        format("Objetivo: ~s~n", [AvaliacaoFisica.objetivo])
    ;   writeln("Avaliacao fisica nao existe!")
    ).

remover_avaliacao_fisica_por_cpf(CPF) :-
    
    (   avaliacao_fisica_existe(CPF)
    ->  string_to_atom(CPF, CPFa),
        atom_concat('BD/avaliacao_fisica/', CPFa, Temp),
        atom_concat(Temp, '.json', Arquivo),
        delete_file(Arquivo),
        writeln("Avaliacao fisica removida com sucesso!")
    ;   writeln("Avaliacao fisica nao existe!")
    ).

listar_todas_avaliacoes_fisicas(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json_avaliacao_fisica(Lista_Arquivos, Path).

processar_arquivos_json_avaliacao_fisica([], _).
processar_arquivos_json_avaliacao_fisica([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_avaliacao_fisica(Caminho),
        processar_arquivos_json_avaliacao_fisica(Arquivos, Diretorio)
    ;   processar_arquivos_json_avaliacao_fisica(Arquivos, Diretorio)
    ).

ler_e_mostrar_avaliacao_fisica(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, AvaliacaoFisica),
    close(Stream),
    format("CPF: ~s~n", [AvaliacaoFisica.cpf]),
    format("Data da Avaliacao: ~s~n", [AvaliacaoFisica.dataAvaliacao]),
    format("Peso: ~s~n", [AvaliacaoFisica.peso]),
    format("Altura: ~s~n", [AvaliacaoFisica.altura]),
    format("Idade: ~s~n", [AvaliacaoFisica.idade]),
    format("Objetivo: ~s~n", [AvaliacaoFisica.objetivo]),
    writeln('').

atualizarAvaliacaoPorCPF(CPF, NumeroCampo, NovoValor) :-
    
    (avaliacao_fisica_existe(CPF) ->

        (   atom_concat('BD/avaliacao_fisica/', CPF, Temp),
            atom_concat(Temp, '.json', Arquivo),
            open(Arquivo, read, Stream),
            json_read_dict(Stream, AvaliacaoFisica),
            close(Stream),

            (   NumeroCampo = 1 ->
                        AvaliacaoAtualizada = AvaliacaoFisica.put(dataAvaliacao, NovoValor)
                ;   NumeroCampo = 2 ->
                        AvaliacaoAtualizada = AvaliacaoFisica.put(peso, NovoValor)
                ;   NumeroCampo = 3 ->
                        AvaliacaoAtualizada = AvaliacaoFisica.put(altura, NovoValor)
                ;   NumeroCampo = 4 ->
                        AvaliacaoAtualizada = AvaliacaoFisica.put(idade, NovoValor)
                ;   NumeroCampo = 5 ->
                        AvaliacaoAtualizada = AvaliacaoFisica.put(objetivo, NovoValor)
            ),

            writeln('\nAtualizando...'),
            sleep(2),
    

            open(Arquivo, write, StreamWrite),
            json_write(StreamWrite, AvaliacaoAtualizada),
            close(StreamWrite),

            writeln('\nFuncionario Atualizado!'),
            sleep(2)
        )
    ;   writeln("Avaliacao nao existe!")
    ).


   



      







