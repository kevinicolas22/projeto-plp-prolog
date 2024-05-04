:- module(mainGestor, [menu_gestor/1, menu_maquina_g/1]).

:- use_module(util).
:- use_module(maquina).
:- use_module(text_util).
:- use_module(maquina_service).
:- use_module(system).
:- use_module(gestor).
:- use_module(funcionario).
:- use_module(library(ansi_term)).
:- use_module('GestorService').
:- use_module('MaquinaService').
:- use_module('FuncionarioService', [
        adicionar_funcionario/1, 
        criar_funcionario/0, 
        ler_funcionario/1, 
        remover_funcionario/1, 
        listar_todos_funcionarios/1, 
        atualizarFuncionarioPorCPF/3]). 

:- use_module('mainPrincipal', [main/0]).

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
        menu_funcionario_g
    ; Opcao = "3" ->
        menu_maquina_g(MenuPrincipal)
    ; Opcao = "4" ->
        menu_financeiro_g(MenuPrincipal)
    ; Opcao = "5" ->
        main
    ;
        writeln('Opção inválida. Por favor, escolha novamente.'),
        menu_gestor(MenuPrincipal),
        fail
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
            criar_gestor_g(MenuPrincipal)
    ;   Opcao = "2" ->
            atualizar_gestor_opcao(MenuPrincipal)
    ;   Opcao = "3" ->
            ler_todos_gestores(MenuPrincipal)
    ;   Opcao = "4" ->
            consultar_gestor_opcao(MenuPrincipal)
    ;   Opcao = "5" ->
            remover_gestor_opcao(MenuPrincipal)
    ;   Opcao = "6" ->
            menu_gestor(MenuPrincipal)
    ;
            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_gestor_g(MenuPrincipal)
    ).

criar_gestor_g(MenuPrincipal) :-
        criar_gestor.

atualizar_gestor_opcao(MenuPrincipal) :-
    writeln('>> Digite o CPF do gestor que deseja atualizar:'),
    read_line_to_string(user_input, CPFG),
    writeln('----------------------------------------------'),
    writeln('|     Escolha o dado do gestor a ser         |'),
    writeln('|              atualizado:                   |'),
    writeln('|                                            |'),
    writeln('|   [1] Nome                                 |'),
    writeln('|   [2] CPF                                  |'),
    writeln('|   [3] Endereço                             |'),
    writeln('|   [4] Telefone                             |'),
    writeln('|   [5] Data de Nascimento                   |'),
    writeln('|                                            |'),
    writeln('|   [0] Voltar                               |'),
    writeln('----------------------------------------------'),
    writeln('Escolha: '),
    read_line_to_string(user_input, Escolha),
    atom_number(Escolha, Numero),
    (   Numero >= 0, Numero =< 6 ->
        (   Numero = 0 ->
                menu_gestor_g(MenuPrincipal)
            ;   
                writeln('Insira o novo valor: '),
                read_line_to_string(user_input, NovoValor),
                atualizar_gestor_porCpf(CPFG, Numero, NovoValor),
                menu_gestor(MenuPrincipal)
        )

    ;   writeln('Opção inválida.'),
        atualizar_gestor_opcao(MenuPrincipal)
    ).


ler_todos_gestores(MenuPrincipal) :-
    writeln('------------GESTORES------------'),
    listar_gestores('BD/gestor'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_gestor_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_todos_gestores(MenuPrincipal)
    ).

consultar_gestor_opcao(MenuPrincipal) :-
    writeln('>> Digite o CPF do gestor que deseja buscar:'),
    read_line_to_string(user_input, CPFG),
    writeln('Procurando...\n'),
    sleep(2),
    consultar_gestor(CPFG), % Alteração feita aqui
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_gestor_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        consultar_gestor_opcao(MenuPrincipal)
    ).

remover_gestor_opcao(MenuPrincipal) :-
        writeln('>> Digite o CPF do gestor que deseja remover:'),
        read_line_to_string(user_input, CPFG),
        writeln('Removendo...\n'),
        sleep(2),
        remover_gestor(CPFG),
        writeln('\n\n [0] Voltar'),
        read_line_to_string(user_input, Op),
        (   Op = "0" ->
                menu_gestor_g(MenuPrincipal)
        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                remover_gestor_opcao(MenuPrincipal)
        ).


     % MAQUINA 

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
        writeln('|   [5] Verificar datas de manutencao de equipamentos |'), 
        writeln('|   [6] Verificar quantidade de maquinas cadastradas  |'),
        writeln('|   [7] Consultar maquina especifica                  |'),
        writeln('|   [8] Remover maquina cadastrada                    |'),
        writeln('|   [9] Remover maquina em manutencao                 |'),
        writeln('|   [10] Atualizar maquina                            |'),
        writeln('|   [11] Voltar para o menu                           |'),
        writeln('|                                                     |'),
        writeln('|   > Digite a opcao:                                 |'),
        writeln('-------------------------------------------------------'),
        read_line_to_string(user_input, Opcao),
        escolher_opcao_maquina_g(Opcao, MenuPrincipal).
escolher_opcao_maquina_g(Opcao, MenuPrincipal) :-
    (   Opcao = "1" ->
            criar_maquina(MenuPrincipal)
    ;   Opcao = "2" ->
            add_no_reparo(MenuPrincipal)
    ;   Opcao = "3" ->
           ler_todas_maquinas(MenuPrincipal)
    ;   Opcao = "4" ->
            ler_todas_maquinas_reparo(MenuPrincipal)
    ;   Opcao = "5" ->
            verificar_data_manutencao(MenuPrincipal)
    ;   Opcao = "6" ->
            quantidade_de_maquinas(MenuPrincipal)
    ;   Opcao = "7" ->
            consultar_maquina_opcao(MenuPrincipal)
    ;   Opcao = "8" ->
            remover_maquina_opcao(MenuPrincipal) 
    ;   Opcao = "9" ->
            remover_maquina_m_opcao(MenuPrincipal)
    ;   Opcao = "10" ->
            atualizar_maquina_opcao(MenuPrincipal)
    ;   Opcao = "11" ->
            menu_gestor(MenuPrincipal)
    ;
            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_maquina_g(MenuPrincipal)
    ).

%criar maquina
criar_maquina(MenuPrincipal) :-
        MaquinaService : criar_maquina(NovaMaquina),
        MaquinaService:adicionar_maquina(NovaMaquina),
        sleep(2),
        menu_maquina_g(MenuPrincipal).
%atualizar dados maquina
atualizar_maquina_opcao(MenuPrincipal) :-
    writeln('>> Digite o codigo da maquina que deseja atualizar:'),
    read_line_to_string(user_input, CodigoM),
    writeln('----------------------------------------------'),
    writeln('|     Escolha o dado do gestor a ser         |'),
    writeln('|              atualizado:                   |'),
    writeln('|                                            |'),
    writeln('|   [1] Nome                                 |'),
    writeln('|   [2] Data de menutenção                   |'),
    writeln('|                                            |'),
    writeln('|   [0] Voltar                               |'),
    writeln('----------------------------------------------'),
    writeln('Escolha: '),
    read_line_to_string(user_input, Escolha),
    atom_number(Escolha, Numero),
    (   Numero >= 0, Numero =< 2 ->
        (   Numero = 0 ->
                menu_maquina_g(MenuPrincipal)
            ;   
                writeln('Insira o novo valor: '),
                read_line_to_string(user_input, NovoValor),
                atualizar_maquina_porCodigo(CodigoM, Numero, NovoValor),
                menu_maquina_g(MenuPrincipal)
        )

    ;   writeln('Opção inválida.'),
        atualizar_maquina_opcao(MenuPrincipal),
        writeln('\n\n [0] Voltar'),
        read_line_to_string(user_input, Op),
        (   Op = "0" ->
                menu_maquina_g(MenuPrincipal)
        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                atualizar_maquina_opcao(MenuPrincipal)
        )
    ). 

%remover maquina
remover_maquina_opcao(MenuPrincipal) :-
        writeln('>> Digite o codigo da maquina que deseja remover:'),
        read_line_to_string(user_input, Codigo),
        writeln('Removendo...\n'),
        sleep(2),
        remover_maquina(Codigo),
        writeln('\n\n [0] Voltar'),
        read_line_to_string(user_input, Op),
        (   Op = "0" ->
                menu_maquina_g(MenuPrincipal)
        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                remover_maquina_opcao(MenuPrincipal)
        ).

%remover maquina de manutenção
remover_maquina_m_opcao(MenuPrincipal) :-
        writeln('>> Digite o codigo da maquina que deseja remover:'),
        read_line_to_string(user_input, CodigoM),
        writeln('Removendo...\n'),
        sleep(2),
        remover_maquina_manutencao(CodigoM),
        writeln('\n\n [0] Voltar'),
        read_line_to_string(user_input, Op),
        (   Op = "0" ->
                menu_maquina_g(MenuPrincipal)
        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                remover_maquina_m_opcao(MenuPrincipal)
        ).

% lista todas as maquinas
ler_todas_maquinas(MenuPrincipal) :-
    writeln('-----------MAQUINAS-----------'),
    listar_maquinas('BD/maquina'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_maquina_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_todas_maquinas(MenuPrincipal)
    ).

% adiciona em reparo
add_no_reparo(MenuPrincipal) :-
        adicionar_maquina_reparo(MenuPrincipal),
        sleep(2),
        writeln('\n\n [0] Voltar'),
        read_line_to_string(user_input, Op),
        (   Op = "0" ->
                menu_maquina_g(MenuPrincipal)
        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                add_no_reparo(MenuPrincipal)
        ).

% lista maquinas em reparo
ler_todas_maquinas_reparo(MenuPrincipal) :-
    writeln('-----------MAQUINAS COM NECESSIDADE DE REPARO-----------'),
    listar_maquinas_R('BD/reparo'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_maquina_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_todas_maquinas(MenuPrincipal)
    ).

%funcao que retorna quantas maquinas existem
quantidade_de_maquinas(MenuPrincipal) :-
    contar_maquinas,
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_maquina_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        contar_maquinas(MenuPrincipal)
    ).

% imprime datas de manutencao
verificar_data_manutencao(MenuPrincipal) :-
    writeln('-----------DATAS DE MANUTENCAO-----------'),
    listar_datas('BD/maquina'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_maquina_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        verificar_data_manutencao(MenuPrincipal)
    ).

% retorna uma maquina especifica
consultar_maquina_opcao(MenuPrincipal) :-
    writeln('>> Digite o codigo da maquina que deseja buscar:'),
    read_line_to_string(user_input, CodigoMaquina),
    writeln('Procurando...\n'),
    sleep(2),
    consultar_maquina(CodigoMaquina), % Alteração feita aqui
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_maquina_g(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        consultar_maquina_opcao(MenuPrincipal)
    ).


    % FINANCEIRO 

    :- use_module(library(system)).

    menu_financeiro_g(MenuPrincipal) :-
        writeln('----------------------------------------------'),
        writeln('            Financeiro Codefit                '),
        writeln('----------------------------------------------'),
        writeln('|                                            |'),
        writeln('|   [1] Folha de Pagamento do Funcioanrio    |'),
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
    ). 

    folha_pagamento_func(MenuPrincipal) :-
        writeln('>> Digite o CPF do funcionario que deseja consultar:'),
        read_line_to_string(user_input, CPF),
        writeln('Consultando...\n'),
        sleep(2),
        imprimir_folha_pagamento(CPF),
        writeln('\n\n [0] Voltar'),
        read_line_to_string(user_input, Op),
        (   Op = "0" ->
                menu_financeiro_g(MenuPrincipal)
        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                folha_pagamento_func(MenuPrincipal)
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
            menu_gestor(MenuPrincipal)
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
                menu_gestor(MenuPrincipal)
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