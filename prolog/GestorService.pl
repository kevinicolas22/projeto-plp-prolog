:- module(GestorService, [
    inicializar_arquivo_json/0,
    adicionar_gestor/2,
    criar_gestor/1,
    atualizar_gestor_porCpf/3,
    listar_gestores/1,
    consultar_gestor/1,
    imprimir_folha_pagamento/1,
    gerar_relatorio/0,
    remover_gestor/1
    
]).

:- use_module(library(http/json)).
:- use_module(library(apply)).

inicializar_arquivo_json :-
    open('gestor.json', write, Stream),
    json_write(Stream, _{}),
    close(Stream).

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
        menu_gestor(MenuPrincipal)
    ).

criar_gestor(MenuPrincipal) :-
    writeln("Digite o CPFG (11 Digitos): "),
    read_line_to_string(user_input, CPFG),
    (   gestor_existe(CPFG)
    ->  writeln("Gestor já existe!"),
        menu_gestor(MenuPrincipal)
    ;   writeln("Nome do Gestor: "),
        read_line_to_string(user_input, NomeG),
        writeln("Digite o endereco: "),
        read_line_to_string(user_input, EnderecoG),
        writeln("Digite o telefone 11 digitos ex: 08391234567"),
        read_line_to_string(user_input, TelefoneG),
        writeln("Digite a data de nascimento 8 digitos ex: DDMMAAAA"),
        read_line_to_string(user_input, DataNascimentoG),
        NovoGestor = gestor{ nomeG: NomeG, cpfG: CPFG, enderecoG: EnderecoG, telefoneG: TelefoneG, dataNascimentoG: DataNascimentoG },
        adicionar_gestor(NovoGestor, MenuPrincipal),
        writeln("Gestor cadastrado com sucesso:"),
        writeln("Nome: " + NomeG),
        writeln("CPFG: " + CPFG),
        writeln("Endereco: " + EnderecoG),
        writeln("Telefone: " + TelefoneG),
        writeln("Data de Nascimento: " + DataNascimentoG)
    ).


atualizar_gestor_porCpf(CPFG, NumeroCampo, NovoValor) :-
    
    (gestor_existe(CPFG) ->

        (   atom_concat('BD/gestor/', CPFG, Temp),
            atom_concat(Temp, '.json', Arquivo),
            open(Arquivo, read, Stream),
            json_read_dict(Stream, Gestor),
            close(Stream),

            (   NumeroCampo = 1 ->
                        GestorAtualizado = Gestor.put(nomeG, NovoValor)
                ;   NumeroCampo = 2 ->
                        delete_file(Arquivo),
                        GestorAtualizado = Gestor.put(cpfG, NovoValor),
                        adicionar_gestor(GestorAtualizado, MenuPrincipal)
                ;   NumeroCampo = 3 ->
                        GestorAtualizado = Gestor.put(enderecoG, NovoValor)
                ;   NumeroCampo = 4 ->
                        GestorAtualizado = Gestor.put(telefoneG, NovoValor)
                ;   NumeroCampo = 5 ->
                        GestorAtualizado = Gestor.put(dataNascimentoG, NovoValor)
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
    (   gestor_existe(CPFG)
      %string_to_atom(CPFG,CPfG)
    ->  atom_concat('BD/gestor/', CPFG, Temp),
        atom_concat(Temp, '.json', Arquivo),
        delete_file(Arquivo),
        writeln("Gestor removido com sucesso!")
    ;   writeln("Gestor nao existe!")
    ).


/*imprimir_folha_pagamento(CPF) :-
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
    exists_file(Arquivo).*/

% gerar relatorio que calcula o valor total que a acdemia recebe(valor, plano), quantidade de alunos em cada plano, media de pagamento por aluno, receita total.





