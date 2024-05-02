:- set_prolog_flag(encoding, utf8).
:- module(mainPrincipal, [main/0]).

:- use_module(util).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(thread)).

:- use_module(mainFuncionario, [menu_funcionario/0]).
:- use_module(mainGestor, [menu_gestor/1]).


:- initialization(main).

main :-
    
    writeln('_________________________________________________________'),
    writeln('               Seja Bem-vindo a CodeFit                  '),
    writeln('_________________________________________________________'),
    writeln('|   1. ALUNO'),
    writeln('|   2. GESTOR'),
    writeln('|   3. FUNCIONÁRIO'),
    writeln('|'),
    write('| > Tipo de usuario (''!'' Para Sair): '),
    flush_output,
    read_line_to_codes(user_input, TipoUsuarioCodes),
    atom_chars(TipoUsuario, TipoUsuarioCodes),  
    
    (TipoUsuario = '!' ->
        writeln("Encerrando..."),
        halt
    ;
        atom_number(TipoUsuario, TipoUsuarioInt),
        tipo_usuario_correto(TipoUsuarioInt, TipoUsuarioValidado),
        (   TipoUsuarioValidado =:= 1 ->
            writeln('login_aluno')
        ;   loginMembro(TipoUsuarioValidado)
        
        )

    ).

       
loginMembro(TipoUsuario):-
    
    writeln('_________________________________________________________'),
    writeln('               Seja Bem-vindo a CodeFit                  '),
    writeln('_________________________________________________________'),
    writeln('__________________________LOGIN__________________________'),
    writeln('Digite o seu cpf (! para sair): '),
    read_line_to_string(user_input, CPF),


    (CPF = '!' ->
        writeln("Encerrando..."),
        halt
    ;
        writeln('Digite sua senha: '),
        read_line_to_string(user_input, Senha),
        writeln('Carregando ...'),
        validadorLogin(CPF, TipoUsuario, Senha),
        sleep(2)

    ).


%Função para validar se login já existe
validadorLogin(Cpf, TipoUsuarioV, SenhaV):-
    atom_string(SenhaV, SenhaString),
    concat_atom(["BD/login/", Cpf, ".json"], Path),
    (exists_file(Path) -> 
        (
            ler_json(Path, _{cpf : CPFj, senha: Senhaj, tipoUsuario: Tipoj}),
            (Senhaj = SenhaString, Tipoj = TipoUsuarioV ->
            writeln("LOGIN CADASTRADO"),
            menu_usuario(TipoUsuarioV)
            ;
            writeln("SENHA OU TIPO DE USUARIO INVALIDOS, TENTE NOVAMENTE!"),
            sleep(3),
            loginMembro(TipoUsuarioV)
            )
        )
    ;
        writeln("LOGIN NÃO CADASTRADO,TENTE NOVAMENTE!"),
        sleep(3),
        main
    ).

%Função para chamar menus   
menu_usuario(TipoUsuario) :-
    (
        TipoUsuario =:= 2 ->
            menu_gestor(MenuPrincipal)
        ;   TipoUsuario =:= 3 ->
            write("func")
    ).


tipo_usuario_correto(X, X) :-
    verificar_int_tipo_funcionario(X).
tipo_usuario_correto(_, TipoCorreto) :-
    writeln('Opcao invalida, tente novamente!'),
    read(NovaOpcao),
    tipo_usuario_correto(NovaOpcao, TipoCorreto).

verificar_int_tipo_funcionario(X) :-
    between(1, 3, X).