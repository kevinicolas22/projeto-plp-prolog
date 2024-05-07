:- module(GestorService, [
    inicializar_arquivo_json/0,
    adicionar_gestor/2,
    criar_gestor/0,
    atualizar_gestor_porCpf/3,
    listar_gestores/1,
    consultar_gestor/1,
    imprimir_folha_pagamento/1,
    gerar_relatorio/0,
    remover_gestor/1,
    criar_login/1,
    contar_alunos/1
]).

:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(util).
:- use_module('alunoService',[
        qnt_alunos_premium/1,
        qnt_alunos_gold/1,
        qnt_alunos_light/1]).

:- use_module(mainGestor, [menu_gestor_g/1]).

inicializar_arquivo_json :-
    open('gestor.json', write, Stream),
    json_write(Stream, _{}),
    close(Stream).

:- dynamic gestor_existe/1.

gestor_existe(CPFG) :-
    atom_concat('BD/gestor/', CPFG, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

adicionar_gestor(NovoGestor, MenuPrincipal) :-
    CPFG = NovoGestor.cpfG,
    (   gestor_existe(CPFG)
    ->  writeln("Gestor já existe!"),
        menu_gestor(MenuPrincipal)
    ;   atom_concat('BD/gestor/', CPFG, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, NovoGestor),
        close(StreamWrite),
        criar_login(NovoGestor),
        menu_gestor(MenuPrincipal)
    ).


criar_login(Gestor):-
    writeln('Digite a senha do login: '),
    read_line_to_string(user_input, SenhaGestor),
    CpfG = Gestor.cpfG,
    NovoLogin = login{
        cpf: CpfG,
        senha: SenhaGestor,
        tipoUsuario: 2
    },
    atom_concat('BD/login/', CpfG, Temp1),
    atom_concat(Temp1, '.json', Arquivo1),
    open(Arquivo1, write, StreamWrite),
    json_write(StreamWrite, NovoLogin),
    close(StreamWrite).

criar_gestor:-
    writeln("Digite o CPF (11 Digitos): "),
    read_line_to_string(user_input, CPFG),
    (   verifica_digitos(CPFG)
    ->  (   gestor_existe(CPFG)
        ->  writeln("Gestor ja existe!"),
            menu_gestor_g(MenuPrincipal)
        ;     
            writeln("Nome do Gestor:"),
            read_line_to_string(user_input, NomeG),
            (   verifica_nao_vazio(NomeG)
            ->  writeln("Digite o endereco: "),
                read_line_to_string(user_input, EnderecoG),
                (   verifica_nao_vazio(EnderecoG)
                ->  writeln("Digite o telefone 11 digitos ex: 08391234567"),
                    read_line_to_string(user_input, TelefoneG),
                    (   verifica_digitos(TelefoneG)
                    ->  writeln("Digite a data de nascimento 8 digitos ex: DDMMAAAA"),
                        read_line_to_string(user_input, DataNascimentoG),
                        (   verifica_data(DataNascimentoG)
                        ->  NovoGestor = gestor{
                                nomeG: NomeG,
                                cpfG: CPFG,
                                enderecoG: EnderecoG,
                                telefoneG: TelefoneG,
                                dataNascimentoG: DataNascimentoG
                                },
                            adicionar_gestor(NovoGestor,MenuPrincipal),
                            writeln("Gestor cadastrado com sucesso:"),
                            writeln("Nome: " + NomeG),
                            writeln("CPFG: " + CPFG),
                            writeln("Endereco: " + EnderecoG),
                            writeln("Telefone: " + TelefoneG),
                            writeln("Data de Nascimento: " + DataNascimentoG)
                        ;   writeln("Data de Nascimento deve estar no formato DDMMAAAA."),
                            menu_gestor_g(MenuPrincipal)
                            )
                        ;   writeln("Telefone deve conter 11 digitos."),
                            menu_gestor_g(MenuPrincipal)
                        )
                    ;   writeln("Endereco nao pode ser vazio."),
                        menu_gestor_g(MenuPrincipal)
                    )
                ;   writeln("Nome nao pode ser vazio."),
                    menu_gestor_g(MenuPrincipal)
                )
        )
        ;   writeln("CPF invalido. Deve conter 11 digitos."),
            menu_gestor_g(MenuPrincipal)
    ).


atualizar_gestor_porCpf(CPFG, NumeroCampo, NovoValor) :-
    
    (gestor_existe(CPFG) ->

        (   atom_concat('BD/gestor/', CPFG, Temp),
            atom_concat(Temp, '.json', Arquivo),
            open(Arquivo, read, Stream),
            json_read_dict(Stream, Gestor),
            close(Stream),

            (   NumeroCampo = 1 ->
                    (   verifica_nao_vazio(NovoValor)
                    ->  GestorAtualizado = Gestor.put(nomeG, NovoValor)
                    ;   writeln("Nome nao pode ser vazio."),
                        menu_gestor_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 2 ->
                        (   verifica_nao_vazio(NovoValor)
                    ->  GestorAtualizado = Gestor.put(cpfG, NovoValor)
                    ;   writeln("Cpf nao pode ser vazio."),
                        menu_gestor_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 3 ->
                        (   verifica_nao_vazio(NovoValor)
                    ->  GestorAtualizado = Gestor.put(enderecoG, NovoValor)
                    ;   writeln("Endereco nao pode ser vazio."),
                        menu_gestor_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 4 ->
                        (   verifica_nao_vazio(NovoValor)
                    ->  GestorAtualizado = Gestor.put(telefoneG, NovoValor)
                    ;   writeln("Telefone nao pode ser vazio."),
                        menu_gestor_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 5 ->
                        (   verifica_nao_vazio(NovoValor)
                    ->  GestorAtualizado = Gestor.put(dataNascimentoG, NovoValor)
                    ;   writeln("Data Nascimento nao pode ser vazio."),
                        menu_gestor_g(MenuPrincipal)
                    )
                    
            ),

            writeln('\nAtualizando...'),
            sleep(2),
    

            open(Arquivo, write, StreamWrite),
            json_write(StreamWrite, GestorAtualizado),
            close(StreamWrite),

            writeln('\nGestor Atualizado!'),
            sleep(2)

        )
    ;   writeln("Gestor nao existe!")
    ).



listar_gestores(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json(Lista_Arquivos, Path).

processar_arquivos_json([], _).
processar_arquivos_json([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_gestor(Caminho),
        processar_arquivos_json(Arquivos, Diretorio)
    ;   processar_arquivos_json(Arquivos, Diretorio)
    ).

ler_e_mostrar_gestor(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Gestor),
    close(Stream),
    format("Nome: ~s~n", [Gestor.nomeG]),
    format("CPF: ~s~n", [Gestor.cpfG]),
    format("Endereco: ~s~n", [Gestor.enderecoG]),
    format("Telefone: ~s~n", [Gestor.telefoneG]),
    format("Data de Nascimento: ~s~n", [Gestor.dataNascimentoG]),
    writeln('').


consultar_gestor(CPFG) :-
    (   gestor_existe(CPFG)
    ->  atom_concat('BD/gestor/', CPFG, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, Gestor),
        close(StreamRead),
        writeln("Informacoes do Gestor:"),
        format("Nome: ~s~n", [Gestor.nomeG]),
        format("CPF: ~s~n", [Gestor.cpfG]),
        format("Endereco: ~s~n", [Gestor.enderecoG]),
        format("Telefone: ~s~n", [Gestor.telefoneG]),
        format("Data de Nascimento: ~s~n", [Gestor.dataNascimentoG])
    ;   writeln("Gestor nao existe!")
    ).


remover_gestor(CPFG) :-
    gestor_existe(CPFG),
    atom_concat('BD/gestor/', CPFG, Temp),
    atom_concat(Temp, '.json', Arquivo),
    atom_concat('BD/login/', CPFG, Temp1),
    atom_concat(Temp1, '.json', Arquivo1),
    delete_file(Arquivo),
    delete_file(Arquivo1),
    retractall(gestor_existe(CPFG)),
    writeln("Gestor removido com sucesso!").

remover_gestor(_) :-
    writeln("Gestor nao existe ou nao é possível remover, pois deve existir pelo menos um gestor.").


imprimir_folha_pagamento(CPF) :-
    funcionario_existe(CPF),
    atom_concat('BD/funcionario/', CPF, Temp),
    atom_concat(Temp, '.json', Arquivo),
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Funcionario),
    close(Stream),
    write("========== FOLHA DE PAGAMENTO "), write(Funcionario.nome), write(" =========="),nl(),
    format(" | Data de Ingresso: ~s~n", [Funcionario.data_ingresso]),
    SalarioBrutoStr = Funcionario.salario,
    number_string(SalarioBruto, SalarioBrutoStr),
    calcularBeneficio(SalarioBruto, Beneficio),
    SalarioTotal = SalarioBruto + Beneficio,
    format(" | Salario Bruto: ~2f~n", [SalarioBruto]),
    format(" | Beneficio: ~2f~n", [Beneficio]),
    format(" | Salario Total: ~2f~n", [SalarioTotal]).
imprimir_folha_pagamento(CPF) :-
    \+ funcionario_existe(CPF),
    writeln("Funcionario nao existe!").

calcularBeneficio(SalarioBruto, Beneficio) :-
    Beneficio is SalarioBruto * 0.1.

funcionario_existe(CPF) :-
    atom_concat('BD/funcionario/', CPF, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

% gerar relatorio que calcula o valor total que a acdemia recebe(valor, plano), quantidade de alunos em cada plano, media de pagamento por aluno, receita total.

alunosPlano(Path) :-
    write("| Alunos plano Premium: " ),
    qnt_alunos_premium(Path),
    write(" | Alunos plano Gold: " ),
    qnt_alunos_gold(Path),
    write(" | Alunos plano Light: " ),
    qnt_alunos_light(Path).

soma_salarios_funcionarios(Path, SomaSalarios) :-
    directory_files(Path, Arquivos),
    processar_arquivos_funcionarios(Arquivos, Path, 0, SomaSalarios).

processar_arquivos_funcionarios([], _, SomaSalarios, SomaSalarios).

processar_arquivos_funcionarios([Arquivo|Arquivos], Diretorio, Acc, SomaSalarios) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    ( string_concat(_, '.json', Arquivo)  ->  
        open(Caminho, read, Stream), 
        json_read_dict(Stream, Funcionario),
        close(Stream),
        (   get_dict(salario, Funcionario, SalarioStr),
            atom_number(SalarioStr, Salario)
        ->  NewAcc is Acc + Salario
        ;   NewAcc is Acc
        ),
        processar_arquivos_funcionarios(Arquivos, Diretorio, NewAcc, SomaSalarios)
    ;
        processar_arquivos_funcionarios(Arquivos, Diretorio, Acc, SomaSalarios)
    ).

soma_pagamentos_planos(Path, SomaPagamentos) :-
    directory_files(Path, Arquivos),
    processar_arquivos_pagamentos(Arquivos, Path, 0, SomaPagamentos).

processar_arquivos_pagamentos([], _, SomaPagamentos, SomaPagamentos).

processar_arquivos_pagamentos([Arquivo|Arquivos], Diretorio, Acc, SomaPagamentos) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    ( string_concat(_, '.json', Arquivo)  ->  
        open(Caminho, read, Stream), 
        json_read_dict(Stream, Pagamento),
        close(Stream),
        (   get_dict(valor, Pagamento, ValorStr),
            atom_number(ValorStr, Valor)
        ->  NewAcc is Acc + Valor
        ;   NewAcc is Acc
        ),
        processar_arquivos_pagamentos(Arquivos, Diretorio, NewAcc, SomaPagamentos)
    ;
        processar_arquivos_pagamentos(Arquivos, Diretorio, Acc, SomaPagamentos)
    ).

receitaLiquida(PathSalarios, PathPagamentos, Diferenca) :-
    soma_salarios_funcionarios(PathSalarios, SomaSalarios), 
    soma_pagamentos_planos(PathPagamentos, SomaPagamentos), 

    Diferenca is SomaPagamentos - SomaSalarios.

contar_alunos(Quantidade) :-
    directory_files('BD/aluno/', Files),
    exclude(system_file, Files, Onlyaluns),
    length(Onlyaluns, Quantidade).

% Predicado auxiliar para excluir arquivos do sistema
system_file(File) :-
    sub_atom(File, 0, _, _, '.').

mediaPagamento(PathPagamentos, Media) :-
    contar_alunos(Quantidade),
    soma_pagamentos_planos(PathPagamentos, SomaPagamentos),
    Media is SomaPagamentos/Quantidade.

gerar_relatorio:-
    write("========== Relatorio Financeiro da CodeFit =========="),nl(),
    alunosPlano('BD/aluno/'), nl(),
    write("| Media de pagamento por aluno: " ),
    mediaPagamento('BD/pagamentos/', Media),
    write(Media), nl(),
    write("| Total de salarios dos funcionarios: " ),
    soma_salarios_funcionarios('BD/funcionario/', SomaSalarios),
    write(SomaSalarios), nl(),
    write("| Receita bruta: " ),
    soma_pagamentos_planos('BD/pagamentos/', SomaPagamentos),
    write(SomaPagamentos), nl(),
    write("| Receita Liquida: "),
    receitaLiquida('BD/funcionario/', 'BD/pagamentos/', Diferenca),
    write(Diferenca), nl().



% VALIDAcÕES

verifica_digitos(CPFG) :-
    atom_length(CPFG, 11).

verifica_data(Data) :-
    atom_length(Data, 8),
    number_chars(_, Data).

verifica_numero_positivo(Numero) :-
    number_codes(Number, Numero),
    all_positive(Number).

all_positive(Number) :-
    Number >= 0.

verifica_nao_vazio(String) :-
    string_length(String, Length),
    Length > 0.



