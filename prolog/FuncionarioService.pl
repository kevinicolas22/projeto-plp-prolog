:- module(FuncionarioService, [
    inicializar_arquivo_json/0,
    adicionar_funcionario/1,
    criar_funcionario/0,
    ler_funcionario/1,
    remover_funcionario/1,
    listar_todos_funcionarios/1,
    atualizarFuncionarioPorCPF/3,
    criar_login/1,
    acesso_liberado/3,
    exibir_solicitacoes/0
]).
:- use_module(library(apply)).

:- use_module(util).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).

:- use_module('alunoService', [
        aluno_existe/1
]).

:- use_module(mainGestor, [menu_funcionario_g/0]).

% Funcão para verificar se um funcionario ja existe
funcionario_existe(CPF) :-
    atom_concat('BD/funcionario/', CPF, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

adicionar_funcionario(NovoFuncionario) :-
    CPF = NovoFuncionario.cpf,
    (   funcionario_existe(CPF)
    ->  writeln("Funcionario ja existe!"),
        menu_funcionario_g
    ;   % Cria o nome do arquivo com base no CPF
        atom_concat('BD/funcionario/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, NovoFuncionario),
        close(StreamWrite),
        criar_login(NovoFuncionario),
        menu_funcionario_g
    ).

criar_login(Funcionario):-
    writeln('Digite a senha do login: '),
    read_line_to_string(user_input, Senha),
    CpfF = Funcionario.cpf,
    NovoLogin = login{
        cpf: CpfF,
        senha: Senha,
        tipoUsuario: 3
    },
    atom_concat('BD/login/', CpfF, Temp1),
    atom_concat(Temp1, '.json', Arquivo1),
    open(Arquivo1, write, StreamWrite),
    json_write(StreamWrite, NovoLogin),
    close(StreamWrite).


criar_funcionario :-
    writeln("Digite o CPF (11 Digitos): "),
    read_line_to_string(user_input, CPF),
    (   verifica_digitos(CPF)
    ->  (   funcionario_existe(CPF)
        ->  writeln("Funcionario ja existe!"),
            menu_funcionario_g
        ;   % Continua com o cadastro do funcionario
            writeln("Nome do Funcionario:"),
            read_line_to_string(user_input, Nome),
            (   verifica_nao_vazio(Nome)
            ->  writeln("Digite o endereco: "),
                read_line_to_string(user_input, Endereco),
                (   verifica_nao_vazio(Endereco)
                ->  writeln("Digite o telefone 11 digitos ex: 08391234567"),
                    read_line_to_string(user_input, Telefone),
                    (   verifica_digitos(Telefone)
                    ->  writeln("Digite a data de ingresso 8 digitos ex: DDMMAAAA"),
                        read_line_to_string(user_input, DataIngresso),
                        (   verifica_data(DataIngresso)
                        ->  writeln("Digite o salario: "),
                            read_line_to_string(user_input, Salario),
                            (   verifica_numero_positivo(Salario)
                            ->  NovoFuncionario = funcionario{
                                    nome: Nome,
                                    cpf: CPF,
                                    endereco: Endereco,
                                    telefone: Telefone,
                                    data_ingresso: DataIngresso,
                                    salario: Salario
                                },
                                adicionar_funcionario(NovoFuncionario),
                                writeln("Funcionario cadastrado com sucesso:"),
                                writeln("Nome: " + Nome),
                                writeln("CPF: " + CPF),
                                writeln("Endereco: " + Endereco),
                                writeln("Telefone: " + Telefone),
                                writeln("Data de Ingresso: " + DataIngresso),
                                writeln("Salario: " + Salario)
                            ;   writeln("Salario deve ser um numero positivo."),
                                menu_funcionario_g
                            )
                        ;   writeln("Data de Ingresso deve estar no formato DDMMAAAA."),
                            menu_funcionario_g
                        )
                    ;   writeln("Telefone deve conter 11 digitos."),
                        menu_funcionario_g
                    )
                ;   writeln("Endereco nao pode ser vazio."),
                    menu_funcionario_g
                )
            ;   writeln("Nome nao pode ser vazio."),
                menu_funcionario_g
            )
        )
    ;   writeln("CPF invalido. Deve conter 11 digitos."),
        menu_funcionario_g
    ).


ler_funcionario(CPF) :-
    (   funcionario_existe(CPF)
    ->  atom_concat('BD/funcionario/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, Funcionario),
        close(StreamRead),
        writeln("Informacoes do Funcionario:"),
        format("Nome: ~s~n", [Funcionario.nome]),
        format("CPF: ~s~n", [Funcionario.cpf]),
        format("Endereco: ~s~n", [Funcionario.endereco]),
        format("Telefone: ~s~n", [Funcionario.telefone]),
        format("Data de Ingresso: ~s~n", [Funcionario.data_ingresso]),
        format("Salario: ~s~n", [Funcionario.salario])
    ;   writeln("Funcionario nao existe!")
    ).


remover_funcionario(CPF) :-
    
    (   funcionario_existe(CPF)
    ->  string_to_atom(CPF, CPFa),
        atom_concat('BD/funcionario/', CPFa, Temp),
        atom_concat(Temp, '.json', Arquivo),
        atom_concat('BD/login/', CPFa, Temp1),
        atom_concat(Temp1, '.json', Arquivo1),
        delete_file(Arquivo1),
        delete_file(Arquivo),
        writeln("Funcionario removido com sucesso!")
    ;   writeln("Funcionario nao existe!")
    ).


listar_todos_funcionarios(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json(Lista_Arquivos, Path).

processar_arquivos_json([], _).
processar_arquivos_json([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_funcionario(Caminho),
        processar_arquivos_json(Arquivos, Diretorio)
    ;   processar_arquivos_json(Arquivos, Diretorio)
    ).

ler_e_mostrar_funcionario(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Funcionario),
    close(Stream),
    format("Nome: ~s~n", [Funcionario.nome]),
    format("CPF: ~s~n", [Funcionario.cpf]),
    format("Endereco: ~s~n", [Funcionario.endereco]),
    format("Telefone: ~s~n", [Funcionario.telefone]),
    format("Data de Ingresso: ~s~n", [Funcionario.data_ingresso]),
    format("Salario: ~s~n", [Funcionario.salario]),
    writeln('').


atualizarFuncionarioPorCPF(CPF, NumeroCampo, NovoValor) :-
    (   funcionario_existe(CPF)
    ->  (   atom_concat('BD/funcionario/', CPF, Temp),
            atom_concat(Temp, '.json', Arquivo),
            open(Arquivo, read, Stream),
            json_read_dict(Stream, Funcionario),
            close(Stream),
            (   NumeroCampo = 1 ->
                    (   verifica_nao_vazio(NovoValor)
                    ->  FuncionarioAtualizado = Funcionario.put(nome, NovoValor)
                    ;   writeln("Nome nao pode ser vazio."),
                        menu_funcionario_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 2 ->
                    (   verifica_nao_vazio(NovoValor)
                    ->  FuncionarioAtualizado = Funcionario.put(endereco, NovoValor)
                    ;   writeln("Endereco nao pode ser vazio."),
                        menu_funcionario_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 3 ->
                    (   verifica_digitos(NovoValor)
                    ->  FuncionarioAtualizado = Funcionario.put(telefone, NovoValor)
                    ;   writeln("Telefone deve conter 11 digitos."),
                        menu_funcionario_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 4 ->
                    (   verifica_data(NovoValor)
                    ->  FuncionarioAtualizado = Funcionario.put(data_ingresso, NovoValor)
                    ;   writeln("Data de ingresso deve estar no formato DDMMAAAA."),
                        menu_funcionario_g(MenuPrincipal)
                    )
                ;   NumeroCampo = 5 ->
                    (   verifica_numero_positivo(NovoValor)
                    ->  FuncionarioAtualizado = Funcionario.put(salario, NovoValor)
                    ;   writeln("Salario deve ser um numero positivo."),
                        menu_funcionario_g(MenuPrincipal)
                    )
            ),
            writeln('\nAtualizando...'),
            sleep(2),
            open(Arquivo, write, StreamWrite),
            json_write(StreamWrite, FuncionarioAtualizado),
            close(StreamWrite),
            writeln('\nFuncionario Atualizado!'),
            sleep(2)
        )
    ;   writeln("Funcionario nao existe!")
    ).

% Liberar acesso

acesso_liberado(Matricula, Hora, R):-

    ler_aluno_retorna_aluno(Matricula, Aluno),

    (plano_existe(Aluno.planoAluno) -> 
        
        ler_e_retora_plano(Aluno.planoAluno, Plano),

        atom_number(Hora, HoraInt),
        atom_number(Plano.horaEntradaMaxima, HoraMaximaInt),
        atom_number(Plano.horaEntradaMinima, HoraMinimaInt),


        ((HoraInt >= HoraMinimaInt) , (HoraInt =< HoraMaximaInt) ->
            R = 1
        ;
            R = 0
        )
    ;

        writeln("Plano invalido!")
        
    
    ).

    
ler_aluno_retorna_aluno(Mat, Aluno):-
    (aluno_existe(Mat) ->
        atom_concat('BD/aluno/', Mat, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, Stream), 
        json_read_dict(Stream, Aluno),
        close(Stream)
    ;
        write("Aluno não encontrado")
    ).

ler_e_retora_plano(PlanoNome, Plano):-
    (plano_existe(PlanoNome) ->
        atom_concat('BD/plano/', PlanoNome, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, Stream), 
        json_read_dict(Stream, Plano),
        close(Stream)
    ;
        write("Plano não encontrado")
    ).

    

plano_existe(Plano) :-
    atom_concat('BD/plano/', Plano, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).


% VALIDAcÕES

verifica_digitos(CPF) :-
    atom_length(CPF, 11).

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

% Predicado para carregar o arquivo JSON
carregar_json(Arquivo, JSON) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, JSON),
    close(Stream).

% Predicado para salvar o JSON de volta ao arquivo
salvar_json(Arquivo, JSON) :-
    open(Arquivo, write, Stream),
    json_write_dict(Stream, JSON),
    close(Stream).

% Predicado para deletar uma solicitação com base na matrícula do aluno e no tipo de treino
deletar_solicitacao(Matricula, TipoTreino, ArquivoEntrada, ArquivoSaida) :-
    carregar_json(ArquivoEntrada, JSON),
    select_dict(_{
        matricula_aluno: Matricula,
        tipo_treino: TipoTreino
    }, JSON.solicitacoes, _, NovasSolicitacoes),
    json_modify(JSON, solicitacoes, _, NovasSolicitacoes, NovoJSON),
    salvar_json(ArquivoSaida, NovoJSON).

exibir_solicitacoes :-
    open('BD/solicitacoes/solicitacoes.json', read, Stream), 
    json_read_dict(Stream, Solicitacao), 
    close(Stream),
    carregar_e_exibir_solicitacoes(Solicitacao).

carregar_e_exibir_solicitacoes(Solicitacao) :-
    exibir_solicitacoes(Solicitacao).

exibir_solicitacoes(Dict) :-

    (   get_dict(solicitacoes, Dict, Solicitacoes),
        % Itera sobre cada treino e exibe seus detalhes
        maplist(exibir_solicitacao, Solicitacoes)

    ;   format("Não há informações de solicitacoes.~n")
    ).

exibir_solicitacao(Solicitacao) :-
    format("\n =>Aluno: ~w~n", [Solicitacao.nome_aluno]),
    format("   Matricula: ~w~n", [Solicitacao.matricula_aluno]),
    format("   Tipo de treino: ~w~n", [Solicitacao.tipo_treino]).

