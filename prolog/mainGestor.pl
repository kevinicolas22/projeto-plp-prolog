:- module(mainGestor, [menu_gestor/1]).

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
:- use_module('FuncionarioService', [adicionar_funcionario/2, criar_funcionario/1, ler_funcionario/1, remover_funcionario/1, listar_todos_funcionarios/1, atualizarFuncionarioPorCPF/3]). 


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

escolher_opcao_gestor(Opcao, _) :-
    (   Opcao = "1" ->
            menu_gestor_g(MenuPrincipal)
    ;   Opcao = "2" ->
            menu_funcionario_g(MenuPrincipal)
    ;   Opcao = "3" ->
            menu_maquina_g(MenuPrincipal)
    ;   Opcao = "4" ->
            menu_financeiro_g(MenuPrincipal)
    ;   Opcao = "5" ->
            writeln('Encerrando o programa...'),
            halt
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_gestor(MenuPrincipal),
            fail 
    ).


% FUNCIONARIO

:- use_module(library(system)).

menu_funcionario_g(MenuPrincipal) :-
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
    escolher_opcao_funcionario_g(Opcao, MenuPrincipal).

escolher_opcao_funcionario_g(Opcao, MenuPrincipal) :-
    (   Opcao = "1" ->
            criar_funcionario(MenuPrincipal)
    ;   Opcao = "2" ->
            atualizar_funcionario_opcao(MenuPrincipal)
    ;   Opcao = "3" ->
            ler_todos_funcionarios(MenuPrincipal)
    ;   Opcao = "4" ->
            ler_funcionario_opcao(MenuPrincipal)
    ;   Opcao = "5" ->
            remover_funcionario_opcao(MenuPrincipal)
    ;   Opcao = "6" ->
            menu_gestor(MenuPrincipal)
    ;
            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_funcionario_g(MenuPrincipal)
    ).


criar_funcionario(MenuPrincipal) :-
    FuncionarioService:criar_funcionario(NovoFuncionario),
    writeln('Nova senha de acesso: '),
    senha_valida(SenhaFunc),
    FuncionarioService:adicionar_funcionario(NovoFuncionario, SenhaFunc),
    sleep(2),
    menu_gestor(MenuPrincipal).

atualizar_funcionario_opcao(MenuPrincipal) :-
    writeln('>> Digite o CPF do funcionário que deseja atualizar:'),
    read_line_to_string(user_input, CPF),
    writeln('----------------------------------------------'),
    writeln('|     Escolha o dado do funcionario a ser    |'),
    writeln('|              atualizado:                   |'),
    writeln('|                                            |'),
    writeln('|   [1] Nome                                 |'),
    writeln('|   [2] CPF                                  |'),
    writeln('|   [3] Endereço                             |'),
    writeln('|   [4] Telefone                             |'),
    writeln('|   [5] Data de Ingresso                     |'),
    writeln('|   [6] Salário                              |'),
    writeln('|                                            |'),
    writeln('|   [0] Voltar                               |'),
    writeln('----------------------------------------------'),
    writeln('Escolha: '),
    read_line_to_string(user_input, Escolha),
    atom_number(Escolha, Numero),
    (   Numero >= 0, Numero =< 6 ->
        (   Numero = 0 ->
                menu_funcionario_g(MenuPrincipal)
            ;   
                writeln('Insira o novo valor: '),
                read_line_to_string(user_input, NovoValor),
                atualizarFuncionarioPorCPF(CPF, Numero, NovoValor),
                menu_gestor(MenuPrincipal)
        )

    ;   writeln('Opção inválida.'),
        atualizar_funcionario_opcao(MenuPrincipal)
    ).

ler_todos_funcionarios(MenuPrincipal) :-
    writeln('------------FUNCIONARIOS------------'),
    listar_todos_funcionarios('BD/funcionario'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_funcionario_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_todos_funcionarios(MenuPrincipal)
    ).

ler_funcionario_opcao(MenuPrincipal) :-
    writeln('>> Digite o CPF do funcionario que deseja buscar:'),
    read_line_to_string(user_input, CPF),
    writeln('Procurando...\n'),
    sleep(2),
    ler_funcionario(CPF), % Alteração feita aqui
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_funcionario_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_funcionario_opcao(MenuPrincipal)
    ).
    
remover_funcionario_opcao(MenuPrincipal) :-
    writeln('>> Digite o CPF do funcionario que deseja remover:'),
    read_line_to_string(user_input, CPF),
    writeln('Removendo...\n'),
    sleep(2),
    remover_funcionario(CPF),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_funcionario_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        remover_funcionario_opcao(MenuPrincipal)
    ).



