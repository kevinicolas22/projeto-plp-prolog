:- module(mainGestor, [menu_gestor/0]).

:- use_module(util).
:- use_module(manager).
:- use_module(manager_service).
:- use_module(maquina).
:- use_module(text_util).
:- use_module(maquina_service).
:- use_module(system).
:- use_module(FuncionarioService).
:- use_module(funcionario).
:- use_module(library(ansi_term)).
:- use_module('FuncionarioService', [
        adicionar_funcionario/1, 
        criar_funcionario/0, 
        ler_funcionario/1, 
        remover_funcionario/1, 
        listar_todos_funcionarios/1, 
        atualizarFuncionarioPorCPF/3]). 

:- use_module('mainPrincipal', [main/0]).

menu_gestor :-
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
    escolher_opcao_gestor(Opcao).

escolher_opcao_gestor(Opcao) :-
    (   Opcao = "1" ->
            menu_gestor_g
    ;   Opcao = "2" ->
            menu_funcionario_g
    ;   Opcao = "3" ->
            menu_maquina_g
    ;   Opcao = "4" ->
            menu_financeiro_g
    ;   Opcao = "5" ->
            main
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_gestor,
            fail 
    ).


% FUNCIONARIO

:- use_module(library(system)).

menu_funcionario_g :-
    writeln('----------------------------------------------'),
    writeln('|           Opcoes sobre Funcionario:        |'),
    writeln('|                                            |'),
    writeln('|   [1] Criar funcionario                    |'),
    writeln('|   [2] Atualizar funcionario                |'),
    writeln('|   [3] Listar funcionario                   |'),
    writeln('|   [4] Consultar funcionario                |'),
    writeln('|   [5] Remover funcionario                  |'), 
    writeln('|   [6] Voltar para o menu                   |'),
    writeln('|                                            |'),
    writeln('|   > Digite a opcao:                        |'),
    writeln('----------------------------------------------'),
    read_line_to_string(user_input, Opcao),
    escolher_opcao_funcionario_g(Opcao).

escolher_opcao_funcionario_g(Opcao) :-
    (   Opcao = "1" ->
            criar_funcionario_g
    ;   Opcao = "2" ->
            atualizar_funcionario_opcao
    ;   Opcao = "3" ->
            ler_todos_funcionarios
    ;   Opcao = "4" ->
            ler_funcionario_opcao
    ;   Opcao = "5" ->
            remover_funcionario_opcao
    ;   Opcao = "6" ->
            menu_gestor
    ;
            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_funcionario_g
    ).


criar_funcionario_g :-
    criar_funcionario.

atualizar_funcionario_opcao :-
    writeln('>> Digite o CPF do funcionario que deseja atualizar:'),
    read_line_to_string(user_input, CPF),
    writeln('----------------------------------------------'),
    writeln('|     Escolha o dado do funcionario a ser    |'),
    writeln('|              atualizado:                   |'),
    writeln('|                                            |'),
    writeln('|   [1] Nome                                 |'),
    writeln('|   [2] Endereco                             |'),
    writeln('|   [3] Telefone                             |'),
    writeln('|   [4] Data de Ingresso                     |'),
    writeln('|   [5] Salario                              |'),
    writeln('|                                            |'),
    writeln('|   [0] Voltar                               |'),
    writeln('----------------------------------------------'),
    writeln('Escolha: '),
    read_line_to_string(user_input, Escolha),
    atom_number(Escolha, Numero),
    (   Numero >= 0, Numero =< 5 ->
        (   Numero = 0 ->
                menu_funcionario_g
            ;   
                writeln('Insira o novo valor: '),
                read_line_to_string(user_input, NovoValor),
                atualizarFuncionarioPorCPF(CPF, Numero, NovoValor),
                menu_gestor
        )

    ;   writeln('Opcao invalida.'),
        menu_funcionario_g
    ).

ler_todos_funcionarios :-
    writeln('------------FUNCIONARIOS------------'),
    listar_todos_funcionarios('BD/funcionario'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_funcionario_g
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_todos_funcionarios
    ).

ler_funcionario_opcao :-
    writeln('>> Digite o CPF do funcionario que deseja buscar:'),
    read_line_to_string(user_input, CPF),
    writeln('Procurando...\n'),
    sleep(2),
    ler_funcionario(CPF), % Alteracao feita aqui
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_funcionario_g
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_funcionario_opcao
    ).
    
remover_funcionario_opcao :-
    writeln('>> Digite o CPF do funcionario que deseja remover:'),
    read_line_to_string(user_input, CPF),
    writeln('Removendo...\n'),
    sleep(2),
    remover_funcionario(CPF),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_funcionario_g
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        remover_funcionario_opcao
    ).