:- module(GestorService, [
    inicializar_arquivo_json/0,
    adicionar_gestor/2,
    criar_gestor/1,
    remover_gestor/1,
    ler_gestor/1
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


ler_gestor(CPFG) :-
    (   gestor_existe(CPFG)
    ->  atom_concat('BD/gestor/', CPFG, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, Gestor),
        close(StreamRead),
        writeln("Informacoes do Gestor:"),
        format("Nome: ~s~n", [Gestor.nomeG]),
        format("CPFG: ~s~n", [Gestor.CPFG]),
        format("Endereco: ~s~n", [Gestor.enderecoG]),
        format("Telefone: ~s~n", [Gestor.telefoneG]),
        format("Data de Nascimento: ~s~n", [Gestor.dataNascimentoG])
    ;   writeln("Gestor nao existe para o CPF fornecido.")
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






