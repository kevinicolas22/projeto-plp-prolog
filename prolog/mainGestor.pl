
:- module(mainGestor, [menu_gestor/1]).

:- use_module(maquina).
:- use_module(text_util).
:- use_module(maquina_service).
:- use_module(system).
:- use_module(gestor).
:- use_module(library(ansi_term)).
:- use_module(gestor_service, [inicializar_arquivo_json/0, adicionar_gestor/2, criar_gestor/1]).

menu_gestor(MenuPrincipal) :-
    writeln('----------------------------------------------'),
    writeln('|   Escolha o assunto da funcionalidade      |'),
    writeln('|   de Gestor:                               |'),
    writeln('|                                            |'),
    writeln('|   [1] Gestor                               |'),
    writeln('|   [2] Funcionario                          |'),
    writeln('|   [3] Maquina                              |'),
    writeln('|   [4] Financeiro                           |'),
    writeln('|   [5] Sair                                 |'),
    writeln('|                                            |'),
    writeln('|   > Digite a opcao:                        |'),
    writeln('----------------------------------------------'),
    read_line_to_string(user_input, Opcao),
    escolher_opcao_gestor(Opcao, MenuPrincipal).

escolher_opcao_gestor(Opcao, MenuPrincipal) :-
    (Opcao = "1" ->
        menu_gestor_g(MenuPrincipal)
    ; Opcao = "2" ->
        menu_Gestor_g(MenuPrincipal)
    ; Opcao = "3" ->
        menu_maquina_g(MenuPrincipal)
    ; Opcao = "4" ->
        menu_financeiro_g(MenuPrincipal)
    ; Opcao = "5" ->
        MenuPrincipal
    ;
        writeln('Opção invalida. Por favor, escolha novamente.'),
        menu_gestor(MenuPrincipal)
    ).

% GESTOR 

menu_gestor_g(MenuPrincipal) :-
    writeln('---------------------------------------------'),
    writeln('          Opcoes Gestor Codefit              '),
    writeln('---------------------------------------------'),
    writeln('|                                            |'),
    writeln('|   [1] Criar Gestor                         |'),
    writeln('|   [2] Atualizar Gestor                     |'),
    writeln('|   [3] Listar Gestor                        |'),
    writeln('|   [4] Consultar Gestor                     |'),
    writeln('|   [5] Remover Gestor                       |'), 
    writeln('|   [6] Voltar para o menu                   |'),
    writeln('|                                            |'),
    writeln('|   > Digite a opcao:                        |'),
    writeln('----------------------------------------------'),
    read_line_to_string(user_input, Opcao),
    escolher_opcao_gestor_g(Opcao, MenuPrincipal).

escolher_opcao_gestor_g(Opcao, MenuPrincipal) :-
    (   Opcao = "1" ->
            criar_gestor(MenuPrincipal)
    ;   Opcao = "2" ->
            atualizar_gestor_opcao(MenuPrincipal)
    ;   Opcao = "3" ->
            ler_todos_gestor(MenuPrincipal)
    ;   Opcao = "4" ->
            ler_gestor_opcao(MenuPrincipal)
    ;   Opcao = "5" ->
            remover_gestor_opcao(MenuPrincipal)
    ;   Opcao = "6" ->
            menu_gestor(MenuPrincipal)
    ;
            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_gestor_g(MenuPrincipal)
    ).

criar_gestor(MenuPrincipal) :-
        GestorService: criar_gestor(NovoGestor),
        writeln('Nova senha de acesso: '),
        senha_valida(SenhaGestor),
        GestorService:adicionar_gestor(NovoGestor, SenhaGestor),
        sleep(2),
        menu_gestor(MenuPrincipal).


    /* % MAQUINA 

    :- use_module(library(system)).

    menu_maquina_g(MenuPrincipal) :-
        writeln('-------------------------------------------------------'),
        writeln('                   Maquina Codefit                     '),
        writeln('-------------------------------------------------------'),
        writeln('|                                                     |'),
        writeln('|   [1] Criar maquina                                 |'),
        writeln('|   [2] Adicionar maquina com necessidade de reparo   |'),
        writeln('|   [3] Listar equipamentos cadastrados               |'),
        writeln('|   [4] Listar maquinas com necessidades de reparo    |'),
        writeln('|   [5] Verificar datas de manutenção dos equipamentos|'), 
        writeln('|   [6] Verificar quantidade de maquinas cadastradas  |'),
        writeln('|   [7] Remover maquina cadastrada                    |'),
        writeln('|   [8] Atualizar maquina                             |'),
        writeln('|   [9] Voltar para o menu                            |'),
        writeln('|                                                     |'),
        writeln('|   > Digite a opcao:                                 |'),
        writeln('-------------------------------------------------------'),
        read_line_to_string(user_input, Opcao),
        escolher_opcao_maquina_g(Opcao, MenuPrincipal).

        escolher_opcao_maquina_g(Opcao, MenuPrincipal) :-
    (   Opcao = "1" ->
            criar_maquina(MenuPrincipal)
    ;   Opcao = "2" ->
            add_maquina_reparo(MenuPrincipal)
    ;   Opcao = "3" ->
            listar_todas_maquinas(MenuPrincipal)
    ;   Opcao = "4" ->
            listar_maquinas_reparo(MenuPrincipal)
    ;   Opcao = "5" ->
            verificar_data_manutencao(MenuPrincipal)
    ;   Opcao = "6" ->
            quantidade_maquina(MenuPrincipal)
    ;
        Opcao = "7" ->
            remover_maquina(MenuPrincipal)
    ;
        Opcao = "8" ->
            atualizar_maquina(MenuPrincipal)
    ;
        Opcao = "9" ->
            menu_gestor(MenuPrincipal)
    ;

            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_maquina_g(MenuPrincipal)
    ).


    % FINANCEIRO 

    :- use_module(library(system)).

    menu_financeiro_g(MenuPrincipal) :-
        writeln('----------------------------------------------'),
        writeln('            Financeiro Codefit                '),
        writeln('----------------------------------------------'),
        writeln('|                                            |'),
        writeln('|   [1] Folha de Pagamento do Gestor    |'),
        writeln('|   [2] Renda e Gastos Mensais               |'),
        writeln('|   [3] Voltar para o menu                   |'),
        writeln('|                                            |'),
        writeln('|   > Digite a opcao:                        |'),
        writeln('----------------------------------------------'),
        read_line_to_string(user_input, Opcao),
        escolher_opcao_financeiro_g(Opcao, MenuPrincipal).

        escolher_opcao_financeiro_g(Opcao, MenuPrincipal) :-
    (   Opcao = "1" ->
            folha_pagamento_func(MenuPrincipal)
    ;   Opcao = "2" ->
            renda_gastos(MenuPrincipal)
    ;   Opcao = "3" ->
            menu_gestor(MenuPrincipal)
    ;
            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_financeiro_g(MenuPrincipal)
    ). */

    








