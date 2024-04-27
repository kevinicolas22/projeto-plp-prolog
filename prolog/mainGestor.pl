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
:- use_module('FuncionarioService', [inicializar_arquivo_json/0, adicionar_funcionario/2, criar_funcionario/1]).


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
        writeln('Opção invalida. Por favor, escolha novamente.'),
        menu_gestor(MenuPrincipal)
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
    FuncionarioService:inicializar_arquivo_json,
    FuncionarioService:criar_funcionario(NovoFuncionario),
    writeln('------------FUNCIONARIO------------'),
    writeln('Nova senha de acesso: '),
    senha_valida(SenhaFunc),
    FuncionarioService:adicionar_funcionario(NovoFuncionario, SenhaFunc),
    writeln('\n Funcionario criado com sucesso!'),
    sleep(2),
    menu_funcionario_g(MenuPrincipal).


ler_todos_funcionarios(MenuPrincipal) :-
    writeln('------------FUNCIONARIOS------------'),
    listar_todos_funcionarios,
    writeln('\n\n [0] Voltar'),
    read(Op),
    (   Op = "0" ->
            menu_funcionario_g(MenuPrincipal)
    ;   writeln('Opção inválida. Por favor, escolha novamente.'),
        ler_todos_funcionarios(MenuPrincipal)
    ).


ler_funcionario_opcao(MenuPrincipal) :-
    writeln('>> Digite o ID do funcionário que deseja buscar:'),
    read(Id),
    writeln('Procurando...\n'),
    sleep(2),
    ler_funcionario_por_id(Id),
    writeln('\n\n [0] Voltar'),
    read(Op),
    (   Op = "0" ->
            menu_funcionario_g(MenuPrincipal)
    ;   writeln('Opção inválida. Por favor, escolha novamente.'),
        ler_funcionario_opcao(MenuPrincipal)
    ).


atualizar_funcionario_opcao(MenuPrincipal) :-
    writeln('>> Digite o ID do funcionário que deseja atualizar:'),
    read(Id),
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
    writeln('----------------------------------------------'),
    read(Escolha),
    (   Escolha = "1" ->
            writeln('>> Digite o novo nome:'),
            read(NovoNome),
            atualizarFuncionarioPorId(Id, Funcionario{funcId: Id, nome: NovoNome, cpf: "", endereco: "", telefone: "", data_ingresso: "", salario: 0.0})
    ;   Escolha = "2" ->
            writeln('>> Digite o novo CPF:'),
            read(NovoCPF),
            atualizarFuncionarioPorId(Id, Funcionario{funcId: Id, nome: "", cpf: NovoCPF, endereco: "", telefone: "", data_ingresso: "", salario: 0.0})
    ;   Escolha = "3" ->
            writeln('>> Digite o novo endereço:'),
            read(NovoEndereco),
            atualizarFuncionarioPorId(Id, Funcionario{funcId: Id, nome: "", cpf: "", endereco: NovoEndereco, telefone: "", data_ingresso: "", salario: 0.0})
    ;   Escolha = "4" ->
            writeln('>> Digite o novo telefone:'),
            read(NovoTelefone),
            atualizarFuncionarioPorId(Id, Funcionario{funcId: Id, nome: "", cpf: "", endereco: "", telefone: NovoTelefone, data_ingresso: "", salario: 0.0})
    ;   Escolha = "5" ->
            writeln('>> Digite a nova data de ingresso:'),
            read(NovaData),
            atualizarFuncionarioPorId(Id, Funcionario{funcId: Id, nome: "", cpf: "", endereco: "", telefone: "", data_ingresso: NovaData, salario: 0.0})
    ;   Escolha = "6" ->
            writeln('>> Digite o novo salário:'),
            read(NovoSalario),
            atualizarFuncionarioPorId(Id, Funcionario{funcId: Id, nome: "", cpf: "", endereco: "", telefone: "", data_ingresso: "", salario: NovoSalario})
    ;   writeln('Opção inválida.')
    ),
    writeln('\nAtualizando...'),
    sleep(2),
    writeln('\nFuncionário Atualizado!'),
    sleep(2),
    menu_funcionario_g(MenuPrincipal).


removerFuncionarioOpcao(MenuPrincipal) :-
    writeln('>> Digite o ID do funcionário que deseja remover:'),
    read(Id),
    removerFuncionarioPorId(Id),
    writeln('Removendo...'),
    sleep(2),
    writeln('Funcionário removido com sucesso!'),
    sleep(2),
    menuFuncionarioG(MenuPrincipal).


