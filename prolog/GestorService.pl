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






