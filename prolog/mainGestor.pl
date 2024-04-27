:- module(mainGestor, [menu_gestor/1]).

:- use_module(manager).
:- use_module(manager_service).
:- use_module(maquina).
:- use_module(text_util).
:- use_module(maquina_service).
:- use_module(system).
:- use_module(funcionario_service).
:- use_module(funcionario).
:- use_module(library(ansi_term)).

% menu do gestor
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
        menu_funcionario_g(MenuPrincipal)
    ; Opcao = "3" ->
        menu_maquina_g(MenuPrincipal)
    ; Opcao = "4" ->
        menu_financeiro_g(MenuPrincipal)
    ; Opcao = "5" ->
        MenuPrincipal
    ;
        writeln('Opção inválida. Por favor, escolha novamente.'),
        menu_gestor(MenuPrincipal)
    ).