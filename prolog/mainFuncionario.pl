:- module(mainFuncionario, [menu_funcionario/0, menu_aulas/0]).

:- use_module(funcionario).
:- use_module(funcionarioService).
:- use_module(avaliacaoFisica).
%:- use_module(treino).
:- use_module(aula).
:- use_module(mainAluno).
:- use_module(aulaService).
:- use_module(aluno).
:- use_module(plano).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(ansi_term)).
:- use_module(library(date)).
:- use_module(library(pure_input)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(date)).

:- use_module('mainPrincipal', [main/0]).

:- use_module('AulaService', [
        criar_aula/0, 
        adicionar_aula/1, 
        ler_aula/1, 
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

:- use_module('alunoService', [
        criar_aluno/0,
        listar_alunos/1,
        aluno_existe/1
]).

menu_funcionario :-
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
    escolher_opcao(Opcao).

escolher_opcao(Opcao) :-
    (   Opcao = "1" -> 
            criar_aluno,
            menu_funcionario
    ;   Opcao = "2" ->
            funcionario_cria_treino(MenuPrincipal)
    ;   Opcao = "3" ->
            listar_todos_alunos
    ;   Opcao = "4" ->
            menu_aulas
    ;   Opcao = "5" ->
            liberar_acesso_aluno
    ;   Opcao = "6" ->
            menu_avaliacao_fisica(MenuPrincipal)
    ;   Opcao = "7" ->
            main
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
            menu_funcionario
    ).

% Liberar acesso do aluno

current_hour_in_24h_format(CurrentHour) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, 'local'),
    date_time_value(hour, DateTime, Hour),
    format(atom(CurrentHour), '~|~`0t~d~2+', [Hour]).

% Formatar data e hora
format_date_time(DateTime, 'D/M/YYYY HH:MM:SS', DateTimeOptions) :-
    format_time(string(DateTime), DateTimeOptions).

% Formatar apenas a hora no formato 24 horas (HH)
format_hour(Hour, 'HH', DateTimeOptions) :-
    format_time(string(Hour), DateTimeOptions).


liberar_acesso_aluno :-
        writeln("Para liberar o acesso do ALUNO, informe a matrícula: (0 para voltar)"),
        repeat,
        read_line_to_string(user_input, Mat),
        (Mat = "0" ->
                menu_funcionario
        ;
                (aluno_existe(Mat)->
                        current_hour_in_24h_format(Hora),
                        write("Horario Atual: "), writeln(Hora),
                        acesso_liberado(Mat, Hora, R),
                        (R =:= 1 ->
                                writeln('\e[92mACESSO AUTORIZADO\e[0m\n'),
                                sleep(2),
                                menu_funcionario
                        ;
                                writeln('\e[91mFORA DO HORARIO\e[0m\n'),
                                sleep(2),
                                menu_funcionario
                        )
                ;
                        writeln("Matricula Invalida, digite novamente "),
                        fail
                )
        ).

        
        



% Lista todos os Alunos

listar_todos_alunos :-
    writeln('------------ALUNOS------------'),
    sleep(2),
    listar_alunos('BD/aluno'),
    writeln('\n\n [0] Voltar'),
    repeat,
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_funcionario
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        fail
    ).

% Avaliacao Fisica

:- use_module(library(system)).

menu_avaliacao_fisica(MenuPrincipal) :-
    writeln('----------------------------------------------'),
    writeln('|         Opcoes sobre Avaliacao Fisica:     |'),
    writeln('|                                            |'),
    writeln('|   [1] Realizar Avaliacao Fisica            |'),
    writeln('|   [2] Buscar Avaliacao por cpf              |'),
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
            menu_funcionario
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

menu_aulas :-
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
                cadastrar_aula
        ;   Opcao = "2" ->
                exibir_aula
        ;   Opcao = "3" ->
                ler_todas_aulas
        ;   Opcao = "4" ->
                excluir_aula
        ;   Opcao = "5" ->
                alterar_aula
        ;   Opcao = "0" ->
                menu_funcionario
        ;   
                writeln('Opção invalida. Por favor, escolha novamente.'),
                menu_aulas
        ).

cadastrar_aula :-
        criar_aula,
        sleep(2),
        menu_aulas.

exibir_aula :-
    writeln("Digite o nome da aula: "),
    read_line_to_string(user_input, Nome),
    writeln("Processando..."),
    (   aula_existe(Nome) ->
                writeln(''),
            sleep(2),
            ler_aula(Nome),
            writeln('\n\n [0] Voltar'),
            read_line_to_string(user_input, Op),
            (   Op = "0" ->
                    menu_aulas
            ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                    exibir_aula
            )
    ;   writeln("Aula não existe!"),
        exibir_aula
    ).

        
ler_todas_aulas :-
    writeln('------------AULAS------------'),
    sleep(2),
    listar_todas_aulas('BD/aula'),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_aulas
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        ler_todas_aulas
    ).

excluir_aula:-
        writeln('>> Digite o Nome da aula que deseja remover:'),
    read_line_to_string(user_input, Nome),
    writeln('Removendo...\n'),
    sleep(2),
    remover_aula(Nome),
    writeln('\n\n [0] Voltar'),
    read_line_to_string(user_input, Op),
    (   Op = "0" ->
            menu_aulas
    ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
        excluir_aula
    ).

alterar_aula :-
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
                        ler_aula(Nome),
                        writeln(''),
                        (  Numero = 1 ->
                                
                                writeln('Insira o novo valor do horario: '),
                                read_line_to_string(user_input, NovoValor),
                                atualizarAulaPorNome(Nome, Numero, NovoValor),
                                menu_aulas
                        ;  Numero = 2 ->
                                writeln("Escolha o(s) novo(s) plano(s)"),
                                planos_permitidos(Planos_Escolhidos),
                                atualizarAulaPorNome(Nome, Numero, Planos_Escolhidos),
                                menu_aulas

                        )
                ;       writeln("Aula nao existe!"),
                       menu_aulas

                )
                

                ;   writeln('Opção inválida.'),
                        writeln('\n [0] Voltar'),
                        read_line_to_string(user_input, Op),
                        (   Op = "0" ->
                                menu_aulas
                        ;   writeln('Opcao invalida. Por favor, escolha novamente.'),
                                alterar_aula
                        )
        ).

