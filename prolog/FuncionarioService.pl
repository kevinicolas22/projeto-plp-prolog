:- module(FuncionarioService, [
    inicializar_arquivo_json/0,
    adicionar_funcionario/2,
    criar_funcionario/1
]).

:- use_module(library(http/json)).
:- use_module(library(apply)). % Para usar max_list/2


inicializar_arquivo_json :-
    open('funcionario.json', write, Stream),
    json_write(Stream, _{}),
    close(Stream).

% Predicado para obter os IDs dos funcionários do arquivo JSON
ids_funcionarios(Conteudo, Ids) :-
    findall(Id, member([id=Id|_], Conteudo), Ids).

gerar_id(Count, Ids, IdFuncionario) :-
    (   member(Count, Ids)
    ->  NextCount is Count + 1,
        gerar_id(NextCount, Ids, IdFuncionario)
    ;   IdFuncionario = Count
    ).

% Predicado para gerar um ID único para um novo funcionário
id_unico(Ids, IdNovo) :-
    (   Ids = []
    ->  IdNovo = 1
    ;   max_list(Ids, Max),
        IdNovo is Max + 1
    ).

adicionar_funcionario(NovoFuncionario, Senha) :-
    open('funcionario.json', read, StreamRead),
    json_read_dict(StreamRead, Conteudo),
    close(StreamRead),
    % Verifica se o ID já existe
    ids_funcionarios(Conteudo, Ids),
    id_unico(Ids, IdNovo),
    % Atualiza o arquivo JSON
    open('funcionario.json', write, StreamWrite),
    json_write(StreamWrite, [NovoFuncionario | Conteudo]),
    close(StreamWrite).

criar_funcionario(NovoFuncionario) :-
    % Abre o arquivo JSON e lê o conteúdo
    open('funcionario.json', read, StreamRead),
    json_read_dict(StreamRead, Conteudo),
    close(StreamRead),
    % Obtém os IDs dos funcionários existentes
    ids_funcionarios(Conteudo, Ids),
    gerar_id(1, Ids, IdFuncionario),
    % Exibe o ID do funcionário
    writeln("------------CADASTRO/FUNCIONRIO------------"),
    writeln("ID: " + IdFuncionario),
    writeln("Nome do Funcionario:"),
    read_line_to_string(user_input, Nome),
    writeln("Digite o CPF (11 Digitos): "),
    read_line_to_string(user_input, CPF),
    writeln("Digite o endereco: "),
    read_line_to_string(user_input, Endereco),
    writeln("Digite o telefone 11 digitos ex: 08391234567"),
    read_line_to_string(user_input, Telefone),
    writeln("Digite a data de ingresso 8 digitos ex: DDMMAAAA"),
    read_line_to_string(user_input, DataIngresso),
    writeln("Digite o salario: "),
    read_line_to_string(user_input, Salario),
    % Cria o termo com todas as informações do funcionário
    NovoFuncionario = funcionario{nome:Nome, cpf:CPF, endereco:Endereco, telefone:Telefone, data_ingresso:DataIngresso, salario:Salario},
    % Adiciona o novo funcionário ao arquivo JSON
    adicionar_funcionario(NovoFuncionario, IdFuncionario).

