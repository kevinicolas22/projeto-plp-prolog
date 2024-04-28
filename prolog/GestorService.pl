:- module(gestor_service, [
    inicializar_arquivo_json/0,
    adicionar_gestor/2,
    criar_gestor/1
]).

:- use_module(library(http/json)).
:- use_module(library(apply)). % Para usar max_list/2

inicializar_arquivo_json :-
    open('gestor.json', write, Stream),
    json_write(Stream, _{}),
    close(Stream).

ids_gestor(Conteudo, Ids) :-
    findall(Id, member([id=Id|_], Conteudo), Ids).

gerar_idG(Count, Ids, IdGestor) :-
    (   member(Count, Ids)
    ->  NextCount is Count + 1,
        gerar_idG(NextCount, Ids, IdGestor)
    ;   IdGestor = Count
    ).

id_unicoG(Ids, IdNovo) :-
    max_list(Ids, MaxId),
    IdNovo is MaxId + 1.

adicionar_gestor(NovoGestor, IdNovo) :-
    open('gestor.json', read, StreamRead),
    json_read_dict(StreamRead, Conteudo),
    close(StreamRead),
    ids_gestor(Conteudo, Ids),
    id_unicoG(Ids, IdNovo),
    open('gestor.json', write, StreamWrite),
    json_write(StreamWrite, [NovoGestor | Conteudo]),
    close(StreamWrite).

criar_gestor(NovoGestor) :-
    open('gestor.json', read, StreamRead),
    json_read_dict(StreamRead, Conteudo),
    close(StreamRead),
    ids_gestor(Conteudo, Ids),
    gerar_idG(1, Ids, IdGestor),
    writeln("------------CADASTRO/GESTOR------------"),
    writeln("ID: " , IdGestor),
    writeln("Digite o nome do gestor: "),
    read_line_to_string(user_input, NomeG),
    writeln("Digite o CPF (11 Digitos): "),
    read_line_to_string(user_input, CPFG),
    writeln("Digite a data de nascimento: "),
    read_line_to_string(user_input, DataNascimentoG),
    writeln("Digite o endereco: "),
    read_line_to_string(user_input, EnderecoG),
    writeln("Digite o telefone 11 digitos ex: 83991234567"),
    read_line_to_string(user_input, TelefoneG),
    writeln('Nova senha de acesso: '),
    read_line_to_string(user_input, SenhaG), % Agora atribuímos um valor a SenhaG
    NovoGestor = gestor{nomeG:NomeG, cpfG:CPFG, dataNascimentoG: DataNascimentoG, enderecoG:EnderecoG, telefoneG:TelefoneG, senhaG:SenhaG},
    adicionar_gestor(NovoGestor, IdGestor).
