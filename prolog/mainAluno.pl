:- module(mainAluno, [login_aluno/0, menu_aluno/1]).
:- use_module(aluno).
:- use_module(aulaService, [listar_todas_aulas/1, aula_existe/1]).
:- use_module(alunoService, [atualizaAlunoPelaMat/3,exibe_avaliacao_aluno/1,carregar_e_exibir_treinos/1, adiciona_solicitacao/2, criar_aluno/0,aluno_existe/1,listar_alunos/1,qnt_alunos_premium/1]).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(process)).
:- use_module(library(smtp)).
:- use_module(pagamento).
:- use_module(mainAluno, [menu_aluno/1]).
:- use_module('mainPrincipal', [main/0]).

login_aluno :- 
    writeln('\n\n==============LOGIN / ALUNO================='),
    login_loop.
    

login_loop :-
    write('> Matricula: '),
    read_line_to_string(user_input, Matricula_login),

    (aluno_existe(Matricula_login) -> 
        write('> Senha: '),
        read_line_to_string(user_input, Senha_login),
        atom_concat('BD/aluno/',Matricula_login,Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, Aluno),
        (Senha_login= Aluno.senhaAluno->         %verificaçao da senha de login
           close(StreamRead),
           writeln('Carregando...'),
           sleep(1.5),
           menu_aluno(Aluno);
            writeln('\e[31mSenha incorreta.\e[0m'),
            sleep(2),
            login_aluno
         ); 
        writeln('Matricula nao encontrada!'),
        format('~c~s~c~s', [0x1b, "[H", 0x1b, "[2J"]),
        login_aluno
    ).

aluno_existe(Matricula):-
    atom_concat('BD/aluno/',Matricula,Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

menu_aluno(Aluno):- 
    writeln('\n -----------------------------------------'),
    write('             Ola, '),format("~s~n", [Aluno.nomeAluno]),
    writeln('                                           '),
    writeln('    [1] Consultar Plano atual               '),
    writeln('    [2] Alterar plano                       '),
    writeln('    [3] Meus Dados                          '),
    writeln('    [4] Aulas coletivas                     '),
    writeln('    [5] Treinos                             '),
    writeln('    [6] Realizar Pagamento                  '),
    writeln('    [7] Recarga de saldo                    '),
    writeln('    [8] Consultar avaliacao fisica          '),
    writeln('    [9] Sair                                '),
    writeln('                                           '),
    writeln('    > Digite a opcao:                       '),
    writeln(' -----------------------------------------'),
    read_line_to_string(user_input,Opcao),
    (Opcao = "1"-> exibe_plano_atual(Aluno);
     Opcao = "2"-> altera_plano(Aluno);
     Opcao = "3"-> dados_aluno(Aluno);
     Opcao = "4"-> aulas_coletivas(Aluno);
     Opcao = "5"-> treinos(Aluno);
     Opcao = "6"-> realiza_pagamento(Aluno);
     Opcao = "7" -> realiza_recarga(Aluno);
     Opcao = "8" -> consulta_avaliacao_fisica(Aluno);
     Opcao = "9" -> writeln('Saindo...\n\n'), sleep(2), main;
     writeln('Opcao invalida!'),
     menu_aluno(Aluno)).


treinos(Aluno):-
    write('\n\n------------- MEUS TREINOS -------------\n'),
    carregar_e_exibir_treinos(Aluno),
    write('\n[0] Voltar     [1] Solicitar treino\n:'),
    read_line_to_string(user_input, Opcao),
    (Opcao = "0"-> menu_aluno(Aluno);
    Opcao = "1" -> 
        write('\n> Tipo de treino: '),
        read_line_to_string(user_input, TipoTreino),
        adiciona_solicitacao(Aluno.matricula, TipoTreino),
        writeln('\e[32m> Treino solicitado com sucesso.\e[0m'),
        sleep(1.5),
        treinos(Aluno)
    ;
    treinos(Aluno)).



consulta_avaliacao_fisica(Aluno):-
    write('\n\n-----------AVALIACAO FISICA-----------'),
    exibe_avaliacao_aluno(Aluno),
    write('\n\n [0] Voltar\n:'),
    read_line_to_string(user_input, Opcao),
    (Opcao="0"-> menu_aluno(Aluno);
    consulta_avaliacao_fisica(Aluno)).


realiza_recarga(Aluno):-
    write('\n\n----------- RECARGA ----------'),
    write('\n\n> Valor da recarga: '),
    read_line_to_string(user_input, Recarga),
    atom_number(Recarga, RecargaNumber),
    atom_number(Aluno.saldo, SaldoAtual),
    NovoSaldo is SaldoAtual + RecargaNumber,
    number_codes(NovoSaldo, Codes),
    atom_codes(Atom, Codes),
    atom_string(Atom, NovoSaldoString),
    ((RecargaNumber < 0; RecargaNumber = 0 ) -> 
        writeln('\e[31m> Valor invalido !\e[0m \n\n'),
        sleep(2),
        realiza_recarga(Aluno);
    writeln('\e[32m> Valor adicionado ao seu saldo.\e[0m'),
    sleep(2),
    atualizaAlunoPelaMat(Aluno.matricula, 15, NovoSaldoString)
    ).
realiza_pagamento(Aluno):-
    atom_concat('BD/plano/',Aluno.planoAluno,Temp),
    atom_concat(Temp, '.json', Arquivo),
    open(Arquivo, read, StreamRead),
    json_read_dict(StreamRead, Plano),
    close(StreamRead),
    write('--------- FINANCEIRO ---------'),
    (Aluno.emDia = "true" -> 
        write('\n\n - Sua mensalidade esta em dia !'),
        write('\n \e[32m>\e[0m Pressione ENTER para voltar...\n'),
        get_single_char(_),
        menu_aluno(Aluno);
    true
    ),
    write('\n\n - Mensalidade pendente.'),
    write('\n - Valor: R$ '), format("~s~n", Plano.valorMensal),
    write('\e[32m - SALDO ATUAL: R$ \e[0m'), format("\e[32m~s~n\e[0m", Aluno.saldo),
    write('\n> Forma de pagamento: '),
    writeln('\n\n[P] Pix'),
    writeln('[D] Cartao de Debito'),
    writeln('[C] Cartao de Credito'),
    writeln('[B] Boleto'),
    write('\n[0] Voltar\n:'),
    read_line_to_string(user_input, Opcao),
    atom_number(Aluno.saldo, SaldoAluno),
    atom_number(Plano.valorMensal, ValorPlano),
    (Opcao = "0"-> menu_aluno(Aluno);
     SaldoAluno< ValorPlano -> writeln('\e[31m> Saldo insuficiente. Efetue uma recarga para prosseguir.\e[0m\n\n'),
     sleep(2),
     realiza_pagamento(Aluno) ;
     writeln('*Processando pagamento...*'),
     sleep(1),
     adiciona_pagamento(Aluno,Plano),
     enviar_email(Aluno, Plano),
     writeln('\e[32mPagamento realizado.\e[0m'),
     sleep(2),
     atualizaAlunoPelaMat(Aluno.matricula, 14, "true")
     ).
     
adiciona_pagamento(Aluno,Plano):-
    NovoPagamento = pagamento{valor: Plano.valorMensal, plano: Plano.tipo},
    atom_concat('BD/pagamentos/', Aluno.matricula, Temp),
    atom_concat(Temp, '.json', Arquivo),
    open(Arquivo, write, StreamWrite),
    json_write(StreamWrite, NovoPagamento),
    close(StreamWrite).

enviar_email(Aluno, Plano) :-
    smtp_send_mail(
         'joao.pedro.arruda.silva@ccc.ufcg.edu.br',
         send_message(Aluno,Plano.tipo,Plano.valorMensal),
        [ security(ssl),
          port(465),
          smtp('smtp.gmail.com'),
          auth('alunocodefit@gmail.com'-'dhvz rvdb bhsv goqu'),
          from('alunocodefit@gmail.com'),
          subject('Pagamento de mensalidade')
        ]
     ).

send_message(Aluno,TipoPlano,Valor,Out) :-
        format(Out, '~s Realizou o pagamento da mensalidade.\n', [ Aluno.nomeAluno ]),
        format(Out, 'Matrícula: ~s\n', [ Aluno.matricula ]),
        format(Out, 'Plano: ~s\n', [  TipoPlano]),
        format(Out, 'Valor: R$ ~s\n', [ Valor  ]).
aulas_coletivas(Aluno):-
    writeln('\n\n--------------- Aulas Coletivas -------------\n'),
    listar_todas_aulas('BD/aula'),
    write('\n[0] Voltar     [1] Inscrever-se    [2] Minhas Aulas\n:'),
    read_line_to_string(user_input, Opcao),
    (Opcao= "0" -> menu_aluno(Aluno);
     Opcao= "1" -> inscricao_aula(Aluno);
     Opcao= "2" -> aulas_aluno(Aluno)
     ).

aulas_aluno(Aluno):-
    write('\n\n------------ MINHAS AULAS ----------\n\n'),
    get_dict(aulasAluno, Aluno, Aulas),
    exibir_lista(Aulas),
    write('\n[0] Voltar      [1] Excluir aula\n:'),
    read_line_to_string(user_input, Opcao),
    (Opcao = "0" -> aulas_coletivas(Aluno);
     Opcao = "1" -> 
        write('\n> Aula a ser excluida: '),
        read_line_to_string(user_input, AulaExc),
        to_lower_case(AulaExc, AulaExcLower),
        (esta_incrito(Aluno.aulasAluno, AulaExcLower)-> true ;
        writeln('\e[31m>\e[0m Voce nao esta inscrito nesta aula.'),
        sleep(2),
        aulas_aluno(Aluno)),
        to_lower_case(AulaExc, AulaExcLower),
        to_upper_case(AulaExc, AulaExcUpper),
        atualizaAlunoPelaMat(Aluno.matricula, 13, AulaExcLower),
        sleep(2),
        aulas_aluno(Aluno)
    ).

esta_incrito([], _):-
    false.
esta_incrito([Aula| Aulas], AulaExc):-
    (Aula = AulaExc -> true;
    esta_incrito(Aulas, AulaExc)).

exibir_lista([]) :- !.
exibir_lista([Aula|Aulas]) :-
    atom_concat('BD/aula/', Aula, Temp),
    atom_concat(Temp, '.json',Arquivo),
    open(Arquivo, read, StreamRead),
    json_read_dict(StreamRead, AulaDict),
    write('-'), write(AulaDict.nome), write(' : '),writeln(AulaDict.horario), write('\n'),
    exibir_lista(Aulas).

inscricao_aula(Aluno):-
    (Aluno.emDia = "false" -> 
        writeln('\n\e[31m>\e[0m Nao eh possivel realizar inscricoes com a mensalidade pendente. Efetue o pagamento...'),
        sleep(3),
        aulas_coletivas(Aluno);
     true),
    write('\n > Nome da aula: '),
    read_line_to_string(user_input, NovaAula),
    to_upper_case(NovaAula, NovaAulaUpper),
    to_lower_case(NovaAula, NovaAulaLower),
    (esta_incrito(Aluno.aulasAluno, NovaAulaLower)-> 
        writeln('\e[31m >\e[0m Voce ja esta inscrito nesta aula.'),
        sleep(2),
        aulas_coletivas(Aluno);
    true),
    (aula_existe(NovaAulaUpper) -> 
        write(NovaAulaUpper), 
        writeln(' adicionada na sua agenda de aulas'),
        sleep(2), 
        atualizaAlunoPelaMat(Aluno.matricula,11,NovaAulaLower),
        aulas_coletivas(Aluno);
    writeln('\n Aula nao encontrada!'),
    sleep(2),
    aulas_coletivas(Aluno)).
    
dados_aluno(Aluno):-
    write('\n------------'), format("~s", [Aluno.nomeAluno]), write('------------\n\n'),
    write('CPF: '), format("~s~n", [Aluno.cpfAluno]),
    write('Endereco: '), format("~s~n", [Aluno.enderecoAluno]),
    write('Contato: '), format("~s~n", [Aluno.contatoAluno]),
    write('Plano: '), format("~s~n", [Aluno.planoAluno]),
    write('Mensalidade: '), 
    (Aluno.emDia = "true"-> write('\e[32mEm dia\e[0m\n');
    write('\e[31mPendente\e[0m\n')),

    write('Matricula: '), format("~s~n", [Aluno.matricula]),
    write('Senha do app: '), format("~s~n", [Aluno.senhaAluno]),
    write('Saldo Atual: R$'), format("~s~n", [Aluno.saldo]),
    write('\n[0] Voltar      [1] Editar Dados\n:'),
    read_line_to_string(user_input, Opcao),
    (Opcao = "0" -> menu_aluno(Aluno)
    ;Opcao = "1" -> editar_dados(Aluno)
    ; dados_aluno(Aluno)).

editar_dados(Aluno):-
    writeln('\n> Dado para edicao: \n'),
    writeln('1. Nome'),
    writeln('2. Endereco'),
    writeln('3. Contato'),
    write('4. Senha do app\n>'),

    read_line_to_string(user_input, Opcao),
    (Opcao = "1" -> 
        write('\n-> Novo nome: '),
        read_line_to_string(user_input, NovoNome),
        writeln('\nAtualizando...'),
        sleep(1),
        writeln('\n\e[32mAtualizado com sucesso!\e[0m'),
        sleep(1),
        atualizaAlunoPelaMat(Aluno.matricula, 1, NovoNome);
    Opcao = "2" ->
        write('\n-> Novo endereco: '),
        read_line_to_string(user_input,NovoEndereco),
        writeln('\nAtualizando...'),
        sleep(1),
        writeln('\n\e[32mAtualizado com sucesso!\e[0m'),
        sleep(1),
        atualizaAlunoPelaMat(Aluno.matricula,2, NovoEndereco);
    Opcao = "3" -> 
        write('\n-> Novo contato: '),
        read_line_to_string(user_input, NovoContato),
        writeln('\nAtualizando...'),
        sleep(1),
        writeln('\n\e[32mAtualizado com sucesso!\e[0m'),
        sleep(1),
        atualizaAlunoPelaMat(Aluno.matricula,3,NovoContato);
    Opcao = "4" -> 
        write('\n-> Nova senha: '),
        read_line_to_string(user_input, NovaSenha),
        writeln('\nAtualizando...'),
        sleep(1),
        writeln('\n\e[32mAtualizado com sucesso!\e[0m'),
        sleep(1),
        atualizaAlunoPelaMat(Aluno.matricula,4,NovaSenha);
    write('> Opcao invalida!'),
    editar_dados(Aluno)).


altera_plano(Aluno):-
    open('BD/plano/gold.json', read, StreamReadG),
    json_read_dict(StreamReadG, PlanoGold),
    close(StreamReadG),
    open('BD/plano/light.json', read, StreamReadL),
    json_read_dict(StreamReadL, PlanoLight),
    close(StreamReadL),
    open('BD/plano/premium.json', read, StreamReadP),
    json_read_dict(StreamReadP, PlanoPremium),
    close(StreamReadP),
    write('\n ------- Planos Codefit ------\n'),
    exibe_plano(PlanoLight),
    exibe_plano(PlanoGold),
    exibe_plano(PlanoPremium),
    write('\n> Novo plano ([0] para voltar): '),
    read_line_to_string(user_input,NovoPlano),
    to_lower_case(NovoPlano, NovoPlanoLower),
     ( NovoPlano= "0" -> menu_aluno(Aluno) 
    ; (NovoPlanoLower = "light"; NovoPlanoLower = "gold" ; NovoPlanoLower= "premium")-> 
        
        writeln('\n\e[32mPlano atualizado.\e[0m'),
        sleep(1),
        atualizaAlunoPelaMat(Aluno.matricula, 12, NovoPlanoLower)
    ; 
      writeln('Plano inexistente!'),
      sleep(2),
      altera_plano(Aluno)).


exibe_plano(Plano):-
    write('\n    ========= '), format("~s", [Plano.tipo]), writeln(' ========'),
    write('Horario Maximo de entrada: '), format("~s", [Plano.horaEntradaMaxima]), writeln(' hrs'),
    write('Horario Minimo de entrada: '), format("~s", [Plano.horaEntradaMinima]), writeln(' hrs'),
    write('Valor Mensal: R$'), format("~s~n", [Plano.valorMensal]).
   
    

exibe_plano_atual(Aluno):-
    atom_concat('BD/plano/',Aluno.planoAluno,Temp),
    atom_concat(Temp, '.json', Arquivo),
    open(Arquivo, read, StreamRead),
    json_read_dict(StreamRead, Plano),
    close(StreamRead),
    write('\n    ========= '), format("~s", [Aluno.planoAluno]), writeln(' ========'),
    write('Horario Maximo de entrada: '), format("~s", [Plano.horaEntradaMaxima]), writeln(' hrs'),
    write('Horario Minimo de entrada: '), format("~s", [Plano.horaEntradaMinima]), writeln(' hrs'),
    write('Valor Mensal: R$'), format("~s~n", [Plano.valorMensal]),
    writeln('\n[0] Sair'),
    read_line_to_string(user_input,Opcao),
    (Opcao="0"-> menu_aluno(Aluno);
     writeln('Opcao invalida!'),
     exibe_plano_atual(Aluno)).

to_lower_case(String, LowerCaseString) :-
    string_lower(String, LowerCaseString).

to_upper_case(String, UpperCaseString) :-
    string_upper(String, UpperCaseString).


