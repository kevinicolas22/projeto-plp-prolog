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

:- use_module('AulaService', [
        criar_aula/1, 
        adicionar_aula/2, 
        ler_aula/2, 
        listar_todas_aulas/1, 
        remover_aula/1, 
        atualizarAulaPorNome/3, 
        planos_permitidos/1, 
        aula_existe/1]).

:- use_module('AvaliacaoService', [
    adicionar_avaliacao_fisica/2,
    criar_avaliacao_fisica/1,
    ler_avaliacao_fisica_por_cpf/1,
    listar_todas_avaliacoes_fisicas/1,
    remover_avaliacao_fisica_por_cpf/1,
    atualizarAvaliacaoPorCPF/3,
    calcular_e_imprimir_imc/1
]).

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
            writeln('Encerrando o programa...'),
            halt
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_gestor(MenuPrincipal),
            fail 
    ).


% Avaliacao Fisica

:- use_module(library(system)).

menu_avaliacao_fisica(MenuPrincipal) :-
    writeln('----------------------------------------------'),
    writeln('|         Opcoes sobre Avaliacao Fisica:     |'),
    writeln('|                                            |'),
    writeln('|   [1] Realizar Avaliacao Fisica            |'),
    writeln('|   [2] Buscar Avaliacao por ID              |'),
    writeln('|   [3] Listar Todas as Avaliacoes Fisicas   |'),
    writeln('|   [4] Atualizar Avaliacao Fisica           |'),
    writeln('|   [5] Verificar IMC                        |'),
    writeln('|   [6] Remover Avaliacao Fisica             |'),
    writeln('|   [7] Voltar ao Menu Principal             |'),
    writeln('|                                            |'),
    writeln('|   > Digite a opcao:                        |'),
    writeln('----------------------------------------------'),
    read_line_to_string(user_input, Opcao),
    escolher_opcao_avaliacao_fisica(Opcao, MenuPrincipal).

escolher_opcao_avaliacao_fisica(Opcao, MenuPrincipal) :-
    (   Opcao = "1" ->
            criar_avaliacao_fisica(MenuPrincipal)
    ;   Opcao = "2" ->
            buscar_avaliacao_por_cpf(MenuPrincipal)
    ;   Opcao = "3" ->
            listar_todas_avaliacoes_fisicas_opcao(MenuPrincipal)
    ;   Opcao = "4" ->
            atualizar_avaliacao_fisica_opcao(MenuPrincipal)
    ;   Opcao = "5" ->
            verificar_imc(MenuPrincipal)
    ;   Opcao = "6" ->
            remover_avaliacao_fisica_opcao(MenuPrincipal)
    ;   Opcao = "7" ->
            menu_funcionario(MenuPrincipal)
    ;
            writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_avaliacao_fisica(MenuPrincipal)
    ).

criar_avaliacao_fisica(MenuPrincipal) :-
    criar_avaliacao_fisica(NovaAvaliacaoFisica) :-
    writeln('Realizando avaliacao fisica...'),
    AvaliacaoService:adicionar_avaliacao_fisica(NovaAvaliacaoFisica, MenuPrincipal), 
    writeln('Avaliacao fisica concluida.'),
    sleep(2),
    menu_avaliacao_fisica(MenuPrincipal).

buscar_avaliacao_por_cpf(MenuPrincipal) :-
    writeln('Digite o CPF do aluno para buscar a avaliacao fisica:'),
    read_line_to_string(user_input, CPF),
    writeln('Procurando...\n'),
    sleep(2),
    ler_avaliacao_fisica_por_cpf(CPF),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
     (   Op = "0" ->
            menu_avaliacao_fisica(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        buscar_avaliacao_por_cpf(MenuPrincipal)
    ).

remover_avaliacao_fisica_opcao(MenuPrincipal) :-
    writeln('Digite o CPF do aluno para remover a avaliacao fisica:'),
    read_line_to_string(user_input, CPF),
    writeln('Removendo...'),
    sleep(2),
    remover_avaliacao_fisica_por_cpf(CPF),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
     (   Op = "0" ->
            menu_avaliacao_fisica(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        buscar_avaliacao_por_cpf(MenuPrincipal)
    ).
      
listar_todas_avaliacoes_fisicas_opcao(MenuPrincipal) :-
    writeln('------------AVALIACOES FISICAS------------'),
    listar_todas_avaliacoes_fisicas('BD/avaliacao_fisica'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_avaliacao_fisica(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        listar_todas_avaliacoes_fisicas_opcao(MenuPrincipal)
    ).

atualizar_avaliacao_fisica_opcao(MenuPrincipal) :-
    writeln('Digite o CPF do aluno para atualizar a avaliacao fisica:'),
    read_line_to_string(user_input, CPF),
    writeln('----------------------------------------------'),
    writeln('|     Escolha o dado da avaliacao fisica a    |'),
    writeln('|              ser atualizado:                |'),
    writeln('|                                            |'),
    writeln('|   [1] Data da Avaliacao                    |'),
    writeln('|   [2] Peso                                 |'),
    writeln('|   [3] Altura                               |'),
    writeln('|   [4] Idade                                |'),
    writeln('|   [5] Objetivo                             |'),
    writeln('|                                            |'),
    writeln('|   [0] Voltar                               |'),
    writeln('----------------------------------------------'),
    writeln('Escolha: '),
    read_line_to_string(user_input, Escolha),
    atom_number(Escolha, Numero),
    (   Numero >= 0, Numero =< 5 ->
        (   Numero = 0 ->
                menu_avaliacao_fisica(MenuPrincipal)
            ;   
                writeln('Insira o novo valor: '),
                read_line_to_string(user_input, NovoValor),
                atualizarAvaliacaoPorCPF(CPF, Numero, NovoValor),
                menu_avaliacao_fisica(MenuPrincipal)
        )

    ;   writeln('Opcao invalida.'),
        atualizar_avaliacao_fisica_opcao(MenuPrincipal)
    ).

verificar_imc(MenuPrincipal) :-
    writeln('Digite o CPF do aluno para verificar o IMC:'),
    read_line_to_string(user_input, CPF),
    writeln('Calculando IMC...'),
    sleep(2),
    calcular_e_imprimir_imc(CPF),
    menu_avaliacao_fisica(MenuPrincipal).

%Aula

menu_aulas(MenuPrincipal) :-
    writeln('---------------------------------------------'),
    writeln('                 Menu Aulas                  '),
    writeln('---------------------------------------------'),
    writeln('|                                           |'),
    writeln('|   [1] Cadastrar Aula                      |'),
    writeln('|   [2] Exibir Aula                         |'),
    writeln('|   [3] Lista de Aulas                      |'),
    writeln('|   [4] Excluir Aula                        |'),
    writeln('|   [5] Alterar Aula                        |'),
    writeln('|                                           |'),
    writeln('|   [0] Voltar                              |'),
    writeln('|                                           |'),
    writeln('|   > Digite a opcao:                       |'),
    writeln('---------------------------------------------'),
    read_line_to_string(user_input, Opcao),
    escolher_opcao_menu_aulas(Opcao, MenuPrincipal).

escolher_opcao_menu_aulas(Opcao, MenuPrincipal) :-
        (   Opcao = "1" -> 
                cadastrar_aula(MenuPrincipal)
        ;   Opcao = "2" ->
                exibir_aula(MenuPrincipal)
        ;   Opcao = "3" ->
                ler_todas_aulas(MenuPrincipal)
        ;   Opcao = "4" ->
                excluir_aula(MenuPrincipal)
        ;   Opcao = "5" ->
                alterar_aula(MenuPrincipal)
        ;   Opcao = "0" ->
                menu_funcionario(MenuPrincipal)
        ;   
                writeln('Opção invalida. Por favor, escolha novamente.'),
                menu_aulas(MenuPrincipal)
        ).

cadastrar_aula(MenuPrincipal) :-
        criar_aula(MenuPrincipal),
        sleep(2),
        menu_aulas(MenuPrincipal).

exibir_aula(MenuPrincipal) :-
    writeln("Digite o nome da aula: "),
    read_line_to_string(user_input, Nome),
    writeln("Processando..."),
    (   aula_existe(Nome) ->
                writeln(''),
            sleep(2),
            ler_aula(Nome, MenuPrincipal),
            writeln('\n\n [0] Voltar'),
            read_line_to_string(user_input, Op),
            (   Op = "0" ->
                    menu_aulas(MenuPrincipal)
            ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                    exibir_aula(MenuPrincipal)
            )
    ;   writeln("Aula não existe!"),
        exibir_aula(MenuPrincipal)
    ).

        
ler_todas_aulas(MenuPrincipal) :-
    writeln('------------AULAS------------'),
    sleep(2),
    listar_todas_aulas('BD/aula'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_aulas(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_todas_aulas(MenuPrincipal)
    ).

excluir_aula(Nome):-
        writeln('>> Digite o Nome da aula que deseja remover:'),
    read_line_to_string(user_input, Nome),
    writeln('Removendo...\n'),
    sleep(2),
    remover_aula(Nome),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_aulas(MenuPrincipal)
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        excluir_aula(MenuPrincipal)
    ).

alterar_aula(MenuPrincipal) :-
        writeln('----------------------------------------------'),
        writeln('|        Escolha o dado da aula a ser        |'),
        writeln('|                 atualizado:                |'),
        writeln('|                                            |'),
        writeln('|   [1] Horario                              |'),
        writeln('|   [2] Planos                               |'),
        writeln('|                                            |'),
        writeln('----------------------------------------------'),
        writeln('Escolha: '),
        read_line_to_string(user_input, Escolha),
        atom_number(Escolha, Numero),

        (   Numero >= 1, Numero =< 2 ->
                writeln('>> Digite o Nome da aula que deseja atualizar:'),
                read_line_to_string(user_input, Nome),
                writeln(''),
                (aula_existe(Nome) ->
                        ler_aula(Nome, MenuPrincipal),
                        writeln(''),
                        (  Numero = 1 ->
                                
                                writeln('Insira o novo valor do horario: '),
                                read_line_to_string(user_input, NovoValor),
                                atualizarAulaPorNome(Nome, Numero, NovoValor),
                                menu_aulas(MenuPrincipal)
                        ;  Numero = 2 ->
                                writeln("Escolha o(s) novo(s) plano(s)"),
                                planos_permitidos(Planos_Escolhidos),
                                atualizarAulaPorNome(Nome, Numero, Planos_Escolhidos),
                                menu_aulas(MenuPrincipal)

                        )
                ;       writeln("Aula nao existe!"),
                       menu_aulas(MenuPrincipal)

                )
                

                ;   writeln('Opção inválida.'),
                        writeln('\n [0] Voltar'),
                        read_line_to_string(user_input, Op),
                        (   Op = "0" ->
                                menu_aulas(MenuPrincipal)
                        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                                alterar_aula(MenuPrincipal)
                        )
        ).