:- module(GestorService, [
    inicializar_arquivo_json/0,
    adicionar_gestor/2,
    criar_gestor/1,
    menu_gestor/1,
    ler_gestor/1,
    remover_gestor/1
]).

:- use_module(library(http/json)).
:- use_module(library(apply)).

:- use_module(mainGestor, [menu_gestor_g/1]).

% Função para verificar se um gestor já existe
gestor_existe(CPF) :-
    atom_concat('BD/gestor/', CPF, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

adicionar_gestor(NovoGestor, MenuPrincipal) :-
    CPF = NovoGestor.cpf,
    (   gestor_existe(CPF)
    ->  writeln("Gestor já existe!"),
        menu_gestor(MenuPrincipal)
    ;   % Cria o nome do arquivo com base no CPF
        atom_concat('BD/gestor/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, NovoGestor),
        close(StreamWrite),
        menu_gestor(MenuPrincipal)
    ).

criar_gestor(MenuPrincipal) :-
    writeln("Digite o CPF (11 Digitos): "),
    read_line_to_string(user_input, CPF),
    % Verifica se o gestor já existe
    (   gestor_existe(CPF)
    ->  writeln("Gestor já existe!"),
        menu_gestor(MenuPrincipal)
    ;   % Continua com o cadastro do gestor
        writeln("Nome do Gestor:"),
        read_line_to_string(user_input, Nome),
        writeln("Digite o endereco: "),
        read_line_to_string(user_input, Endereco),
        writeln("Digite o telefone 11 digitos ex: 08391234567"),
        read_line_to_string(user_input, Telefone),
        writeln("Digite a data de nascimento 8 digitos ex: DDMMAAAA"),
        read_line_to_string(user_input, DataNascimento),
        NovoGestor = gestor{
            nomeG: Nome,
            cpfG: CPF,
            enderecoG: Endereco,
            telefoneG: Telefone,
            dataNascimentoG: DataNascimento
        },
        adicionar_gestor(NovoGestor, MenuPrincipal),
        writeln("Gestor cadastrado com sucesso:"),
        writeln("Nome: " + Nome),
        writeln("CPF: " + CPF),
        writeln("Endereco: " + Endereco),
        writeln("Telefone: " + Telefone),
        writeln("Data de Nascimento: " + DataNascimento)
    ).

ler_gestor(CPF) :-
    (   gestor_existe(CPF)
    ->  atom_concat('BD/gestor/', CPF, Temp),
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


remover_gestor(CPF) :-
    (   gestor_existe(CPF)
    ->  atom_concat('BD/gestor/', CPF, Temp),
        atom_concat(Temp, '.json', Arquivo),
        delete_file(Arquivo),
        writeln("Gestor removido com sucesso!")
    ;   writeln("Gestor nao existe!")
    ).

/*
:- module(GestorService, [
    inicializar_arquivo_json/0,
    adicionar_gestor/5,
    criar_gestor/0
]).

:- use_module(library(http/json)).
:- use_module(library(apply)). % Para usar max_list/2

inicializar_arquivo_json :-
    open('gestor.json', write, Stream),
    json_write(Stream, _{}),
    close(Stream).

ids_gestores(Conteudo, Ids) :-
    findall(Id, member([id=Id|_], Conteudo), Ids).

gerar_idG(Count, Ids, IdGestor) :-
    (   member(Count, Ids)
    ->  NextCount is Count + 1,
        gerar_idG(NextCount, Ids, IdGestor)
    ;   IdGestor = Count
    ).

% Predicado para gerar um ID único para um novo gestor
id_unicoG(Ids, IdNovo) :-
    (   Ids = []
    ->  IdNovo = 1
    ;   max_list(Ids, Max),
        IdNovo is Max + 1
    ).



gestor_existe(CPF) :-
    atom_concat('BD/gestor/', CPFG, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

adicionar_gestor(NovoGestor,MenuPrincipal) :-
    open('gestor.json', read, StreamRead),
    json_read_dict(StreamRead, Conteudo),
    close(StreamRead),
    % Verifica se o ID já existe
    ids_gestores(Conteudo, Ids),
    id_unicoG(Ids, IdNovo),
    % Atualiza o arquivo JSON
    open('gestor.json', write, StreamWrite),
    json_write(StreamWrite, [NovoGestor | Conteudo]),
    close(StreamWrite).

criar_gestor :-
    % Abre o arquivo JSON e lê o conteúdo
    open('gestor.json', read, StreamRead),
    json_read_dict(StreamRead, Conteudo),
    close(StreamRead),
    % Obtém os IDs dos gestores existentes
    ids_gestores(Conteudo, Ids),
    gerar_idG(1, Ids, IdGestor),
    % Exibe o ID do gestor
    writeln("------------CADASTRO/GESTOR------------"),
    writeln("ID: " + IdGestor),
    writeln("Nome do Gestor:"),
    read_line_to_string(user_input, NomeG),
    writeln("Digite o CPF (11 Digitos): "),
    read_line_to_string(user_input, CPFG),
    writeln("Digite a data de nascimento (8 digitos): "),
    read_line_to_string(user_input, DataNascimentoG),
    writeln("Digite o endereco: "),
    read_line_to_string(user_input, EnderecoG),
    writeln("Digite o telefone 11 digitos ex: 08391234567"),
    read_line_to_string(user_input, TelefoneG),
    % Cria o termo com todas as informações do gestor
    NovoGestor = gestor{nome:NomeG, cpf:CPFG, data_nascimento:DataNascimentoG, endereco:EnderecoG, telefone:TelefoneG},
    % Adiciona o novo gestor ao arquivo JSON
    adicionar_gestor(NovoGestor).
*/






