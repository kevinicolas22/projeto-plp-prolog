:- module(FuncionarioService, [
    inicializar_arquivo_json/0,
    adicionar_funcionario/2,
    criar_funcionario/1,
    menu_gestor_funcionario/1,
    ler_funcionario/1,
    remover_funcionario/1
]).

:- use_module(library(http/json)).
:- use_module(library(apply)).

:- use_module(mainGestor, [menu_funcionario_g/1]).

% Função para verificar se um funcionário já existe
funcionario_existe(CPF) :-
    atom_concat('BD/funcionario/', CPF, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

adicionar_funcionario(NovoFuncionario, MenuPrincipal) :-
    CPF = NovoFuncionario.cpf,
    (   funcionario_existe(CPF)
    ->  writeln("Funcionario já existe!"),
        menu_funcionario_g(MenuPrincipal)
    ;   % Cria o nome do arquivo com base no CPF
        atom_concat('BD/funcionario/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, NovoFuncionario),
        close(StreamWrite),
        menu_funcionario_g(MenuPrincipal)
    ).

criar_funcionario(MenuPrincipal) :-
    writeln("Digite o CPF (11 Digitos): "),
    read_line_to_string(user_input, CPF),
    % Verifica se o funcionário já existe
    (   funcionario_existe(CPF)
    ->  writeln("Funcionario ja existe!"),
        menu_funcionario_g(MenuPrincipal)
    ;   % Continua com o cadastro do funcionário
        writeln("Nome do Funcionario:"),
        read_line_to_string(user_input, Nome),
        writeln("Digite o endereco: "),
        read_line_to_string(user_input, Endereco),
        writeln("Digite o telefone 11 digitos ex: 08391234567"),
        read_line_to_string(user_input, Telefone),
        writeln("Digite a data de ingresso 8 digitos ex: DDMMAAAA"),
        read_line_to_string(user_input, DataIngresso),
        writeln("Digite o salario: "),
        read_line_to_string(user_input, Salario),
        NovoFuncionario = funcionario{
            nome: Nome,
            cpf: CPF,
            endereco: Endereco,
            telefone: Telefone,
            data_ingresso: DataIngresso,
            salario: Salario
        },
        adicionar_funcionario(NovoFuncionario, MenuPrincipal),
        writeln("Funcionario cadastrado com sucesso:"),
        writeln("Nome: " + Nome),
        writeln("CPF: " + CPF),
        writeln("Endereco: " + Endereco),
        writeln("Telefone: " + Telefone),
        writeln("Data de Ingresso: " + DataIngresso),
        writeln("Salario: " + Salario)
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
    ->  atom_concat('BD/funcionario/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        delete_file(Arquivo),
        writeln("Funcionario removido com sucesso!")
    ;   writeln("Funcionario nao existe!"),
    ).






    


