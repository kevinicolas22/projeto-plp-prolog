:- module(mainFuncionario, [menu_funcionario/1]).

:- use_module(funcionario).
:- use_module(funcionario_service).
:- use_module(avaliacao_fisica).
:- use_module(treino).
:- use_module(aula).
:- use_module(main_aluno).
:- use_module(aluno_controller).
:- use_module(aula_service).
:- use_module(aluno).
:- use_module(planos).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(ansi_term)).
:- use_module(library(date)).
:- use_module(library(pure_input)).
:- use_module(library(random)).
:- use_module(library(apply)).

menu_funcionario(MenuPrincipal) :-
    writeln('---------------------------------------------'),
    writeln('            Funcionario Codefit             '),
    writeln('---------------------------------------------'),
    writeln('|                                           |'),
    writeln('|   [1] Cadastrar Aluno                     |'),
    writeln('|   [2] Solicitacoes de Treino              |'),
    writeln('|   [3] Lista de Alunos                     |'),
    writeln('|   [4] Menu de Aulas                       |'),
    writeln('|   [5] Liberar Acesso Aluno                |'),
    writeln('|   [6] Menu de Avaliacao Fisica            |'),
    writeln('|   [7] Sair                                |'),
    writeln('|                                           |'),
    writeln('|   > Digite a opcao:                       |'),
    writeln('---------------------------------------------'),
    read_line_to_string(user_input, Opcao),
    escolher_opcao(Opcao, MenuPrincipal).

escolher_opcao(Opcao, MenuPrincipal) :-
    (   Opcao = "1" -> 
            criar_aluno,
            menu_funcionario(MenuPrincipal)
    ;   Opcao = "2" ->
            funcionario_cria_treino(MenuPrincipal)
    ;   Opcao = "3" ->
            listar_alunos(MenuPrincipal)
    ;   Opcao = "4" ->
            menu_aulas(MenuPrincipal)
    ;   Opcao = "5" ->
            liberar_acesso_aluno(MenuPrincipal)
    ;   Opcao = "6" ->
            menu_avaliacao_fisica(MenuPrincipal)
    ;   Opcao = "7" ->
            writeln('Saindo...'),
            MenuPrincipal
    ;   
            writeln('Opção invalida. Por favor, escolha novamente.'),
            menu_funcionario(MenuPrincipal)
    ).
