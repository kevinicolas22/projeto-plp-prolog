:- module(AvaliacaoService, [
    adicionar_avaliacao_fisica/2,
    criar_avaliacao_fisica/1,
    menu_funcionario_avaliacao/1,
    ler_avaliacao_fisica_por_cpf/1,
    listar_todas_avaliacoes_fisicas/1,
    remover_avaliacao_fisica_por_cpf/1,
    atualizarAvaliacaoPorCPF/3,
    calcular_e_imprimir_imc/1,
    calcular_imc/3 
]).

:- use_module(util).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).

:- use_module(mainFuncionario, [menu_avaliacao_fisica/1, atualizar_avaliacao_fisica_opcao/1
]).


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
    (   verifica_cpf(CPF)
    ->  writeln("Data da Avaliacao (DDMMAAAA): "),
        read_line_to_string(user_input, DataAvaliacao),
        (   verifica_data(DataAvaliacao)
        ->  writeln("Peso (kg): "),
            read_line_to_string(user_input, Peso),
            (   verifica_numero_positivo(Peso)
            ->  writeln("Altura (m): "),
                read_line_to_string(user_input, Altura),
                (   verifica_numero_positivo(Altura)
                ->  writeln("Idade: "),
                    read_line_to_string(user_input, Idade),
                    (   verifica_numero_positivo(Idade)
                    ->  writeln("Objetivo: "),
                        read_line_to_string(user_input, Objetivo),
                        (   verifica_nao_vazio(Objetivo)
                        ->  NovaAvaliacaoFisica = avaliacao_fisica{
                                cpf : CPF,
                                dataAvaliacao: DataAvaliacao,
                                peso: Peso,
                                altura: Altura,
                                idade: Idade,
                                objetivo: Objetivo
                            },
                            adicionar_avaliacao_fisica(NovaAvaliacaoFisica, MenuPrincipal),
                            writeln("Avaliacao fisica cadastrada com sucesso:"),
                            writeln("CPF: " + CPF),
                            writeln("Data da Avaliacao: " + DataAvaliacao),
                            writeln("Peso: " + Peso + " kg"),
                            writeln("Altura: " + Altura + " m"),
                            writeln("Idade: " + Idade),
                            writeln("Objetivo: " + Objetivo)
                        ;   writeln("Objetivo nao pode ser vazio."),
                            menu_avaliacao_fisica(MenuPrincipal)
                        )
                    ;   writeln("Idade deve ser um numero inteiro positivo."),
                        menu_avaliacao_fisica(MenuPrincipal)
                    )
                ;   writeln("Altura deve ser um numero positivo."),
                    menu_avaliacao_fisica(MenuPrincipal)
                )
            ;   writeln("Peso deve ser um numero positivo."),
                menu_avaliacao_fisica(MenuPrincipal)
            )
        ;   writeln("Data da Avaliacao deve estar no formato DDMMAAAA."),
            menu_avaliacao_fisica(MenuPrincipal)
        )
    ;   writeln("CPF invalido. Deve conter 11 digitos."),
        menu_avaliacao_fisica(MenuPrincipal)
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
    (   avaliacao_fisica_existe(CPF)
    ->  (   atom_concat('BD/avaliacao_fisica/', CPF, Temp),
            atom_concat(Temp, '.json', Arquivo),
            open(Arquivo, read, Stream),
            json_read_dict(Stream, AvaliacaoFisica),
            close(Stream),
            (   NumeroCampo = 1 ->
                    (   verifica_data(NovoValor)
                    ->  AvaliacaoAtualizada = AvaliacaoFisica.put(dataAvaliacao, NovoValor)
                    ;   writeln("Data da Avaliacao deve estar no formato DDMMAAAA."),
                        menu_avaliacao_fisica(MenuPrincipal)
                    )
                ;   NumeroCampo = 2 ->
                    (   verifica_numero_positivo(NovoValor)
                    ->  AvaliacaoAtualizada = AvaliacaoFisica.put(peso, NovoValor)
                    ;   writeln("Peso deve ser um numero positivo."),
                        menu_avaliacao_fisica(MenuPrincipal)
                    )
                ;   NumeroCampo = 3 ->
                    (   verifica_numero_positivo(NovoValor)
                    ->  AvaliacaoAtualizada = AvaliacaoFisica.put(altura, NovoValor)
                    ;   writeln("Altura deve ser um numero positivo."),
                        menu_avaliacao_fisica(MenuPrincipal)
                    )
                ;   NumeroCampo = 4 ->
                    (   verifica_numero_positivo(NovoValor)
                    ->  AvaliacaoAtualizada = AvaliacaoFisica.put(idade, NovoValor)
                    ;   writeln("Idade deve ser um numero inteiro positivo."),
                        menu_avaliacao_fisica(MenuPrincipal)
                    )
                ;   NumeroCampo = 5 ->
                    (   verifica_nao_vazio(NovoValor)
                    ->  AvaliacaoAtualizada = AvaliacaoFisica.put(objetivo, NovoValor)
                    ;   writeln("Objetivo nao pode ser vazio."),
                        menu_avaliacao_fisica(MenuPrincipal)
                    )
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


calcular_e_imprimir_imc(CPF) :-
    (
        avaliacao_fisica_existe(CPF)
        ->  atom_concat('BD/avaliacao_fisica/', CPF, Temp),
            atom_concat(Temp, '.json', Arquivo),
            open(Arquivo, read, Stream),
            json_read_dict(Stream, AvaliacaoFisica),
            close(Stream),
            atom_number(AvaliacaoFisica.peso, Peso),
            atom_number(AvaliacaoFisica.altura, Altura),
            calcular_imc(Peso, Altura, IMC),
            format('IMC: ~2f~n', [IMC]),
            imprimir_resumo_imc(IMC)
        ;   writeln("Avaliacao fisica nao existe!")
    ).


imprimir_resumo_imc(IMC) :-
    writeln('Resumo do IMC:'),
    (
        IMC < 16 -> writeln('Baixo peso Grau III'), writeln('Recomenda-se procurar um medico imediatamente.')
        ; IMC >= 16, IMC < 17 -> writeln('Baixo peso Grau II'), writeln('Recomenda-se aumentar a ingestao calorica e buscar orientacao medica.')
        ; IMC >= 17, IMC < 18.5 -> writeln('Baixo peso Grau I'), writeln('Recomenda-se aumentar a ingestao calorica e praticar exercicios fisicos regularmente.')
        ; IMC >= 18.5, IMC < 25 -> writeln('Peso normal'), writeln('Parabens! Seu peso esta dentro da faixa saudavel. Continue mantendo habitos saudaveis.')
        ; IMC >= 25, IMC < 30 -> writeln('Sobrepeso'), writeln('Recomenda-se controlar a dieta e aumentar a pratica de exercicios fisicos.')
        ; IMC >= 30, IMC < 35 -> writeln('Obesidade Grau I'), writeln('Recomenda-se uma dieta balanceada e acompanhamento medico regular.')
        ; IMC >= 35, IMC < 40 -> writeln('Obesidade Grau II'), writeln('Recomenda-se buscar orientacao medica para iniciar um programa de perda de peso.')
        ; IMC >= 40 -> writeln('Obesidade Grau III (morbida)'), writeln('Recomenda-se procurar um medico para avaliacao detalhada.')
    ).


calcular_imc(Peso, Altura, IMC) :-
    IMC is Peso / (Altura * Altura).



% VALIDAÇÕES

verifica_cpf(CPF) :-
    atom_length(CPF, 11).

verifica_data(Data) :-
    atom_length(Data, 8),
    number_chars(_, Data).

verifica_numero_positivo(Numero) :-
    number_codes(Number, Numero),
    all_positive(Number).

all_positive(Number) :-
    Number >= 0.

verifica_nao_vazio(String) :-
    string_length(String, Length),
    Length > 0.




   



      







