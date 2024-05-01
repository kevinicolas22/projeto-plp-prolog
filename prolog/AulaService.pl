:- module(AulaService, [
    criar_aula/1,
    adicionar_aula/2,
    ler_aula/2,
    listar_todas_aulas/1,
    remover_aula/1,
    planos_permitidos/1,
    atualizarAulaPorNome/3,
    aula_existe/1
]).

:- use_module(util).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).

:- use_module(mainFuncionario, [menu_aulas/1]).

%Aulas

aula_existe(Nome):-
    atom_concat('BD/aula/', Nome, Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).

planos_permitidos(Planos_Escolhidos) :-
    writeln('---------------------------------------------'),
    writeln('             Opcoes de Planos                '),
    writeln('---------------------------------------------'),
    writeln('|                                           |'),
    writeln('|   [1] Ligth                               |'),
    writeln('|   [2] Gold                                |'),
    writeln('|   [3] Premium                             |'),
    writeln('|                                           |'),
    writeln('---------------------------------------------'),
    writeln('Escolha os planos permitidos (Digite 0 para finalizar):'),
    planos_permitidos_rec([1,2,3], 3, [], Planos_Escolhidos).

planos_permitidos_rec(_, 0, Planos_Escolhidos, Planos_Escolhidos).

planos_permitidos_rec(Planos_Atuais, N, Planos_EscolhidosTemp, Planos_Escolhidos) :-
    N > 0,
    writeln('Digite o número correspondente ao plano desejado: '),
    read_line_to_string(user_input, Opcao1),
    atom_number(Opcao1, Opcao),
    (
        Opcao =:= 0 ->
            (
                length(Planos_EscolhidosTemp, Tamanho),
                Tamanho =:= 0 ->
                    writeln('Pelo menos um plano deve ser escolhido.'),
                    planos_permitidos_rec(Planos_Atuais, N, Planos_EscolhidosTemp, Planos_Escolhidos)
                ;
                    Planos_Escolhidos = Planos_EscolhidosTemp
            )
        ;
            (
                member(Opcao, Planos_Atuais) -> (
                        (   member(Opcao, Planos_EscolhidosTemp) ->
                            writeln('Plano já foi escolhido. Por favor, escolha outro.'),
                            planos_permitidos_rec(Planos_Atuais, N, Planos_EscolhidosTemp, Planos_Escolhidos)
                            ;
                            N1 is N - 1,
                            escolher_plano(Opcao, Plano),
                            append(Planos_EscolhidosTemp, [Plano], NovosPlanos),
                            planos_permitidos_rec(Planos_Atuais, N1, NovosPlanos, Planos_Escolhidos)
                        )
                    )
                ;
                    (
                    writeln('Opcao invalida. Por favor, digite 1, 2, 3 ou 0 para finalizar.'),
                    planos_permitidos_rec(Planos_Atuais, N, Planos_EscolhidosTemp, Planos_Escolhidos)
                    )
                
            )
        
    ).

escolher_plano(1, light).
escolher_plano(2, gold).
escolher_plano(3, premium).

imprimir_planos([]).
imprimir_planos([Plano|Resto]) :-
    writeln(Plano),
    imprimir_planos(Resto).


adicionar_aula(NovaAula, MenuPrincipal):-
    Nome = NovaAula.nome,
    (   aula_existe(Nome)
    ->  writeln("Aula ja existe"),
        sleep(2),
        menu_aulas(MenuPrincipal)
    ;
        make_directory_path('BD/aula/'),
        atom_concat('BD/aula/', Nome, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, write, StreamWrite),
        json_write(StreamWrite, NovaAula),
        close(StreamWrite),
        menu_aulas(MenuPrincipal)
    ).



criar_aula(MenuPrincipal):-
    writeln('Digite o nome da aula: '),
    read_line_to_string(user_input, Nome),
    (   aula_existe(Nome)
    ->  writeln("Aula ja existe!"),
        menu_aulas(MenuPrincipal)
    ;   
        writeln("Digite o horário: "),
        read_line_to_string(user_input, Horario),
        planos_permitidos(Planos_Escolhidos),
        confirmar_dados(Nome, Horario, Planos_Escolhidos, MenuPrincipal)
    ).

confirmar_dados(Nome, Horario, Planos_Escolhidos, MenuPrincipal):-
    writeln(" "),
    writeln("Confirmar Dados:"),
    format('Nome: ~s~n', [Nome]),
    format('Horario: ~s~n', [Horario]),
    writeln('Planos Escolhidos:'),
    imprimir_planos(Planos_Escolhidos),
    writeln(" "),
    writeln("[C] Confirmar   [V] Voltar"),
    read_line_to_string(user_input, Confirmacao),
    (
        Confirmacao = "C"; Confirmacao = "c" ->
            NovaAula = aula{
                nome: Nome,
                horario: Horario,
                planos: Planos_Escolhidos
            },
            adicionar_aula(NovaAula, MenuPrincipal)
        ;
            Confirmacao = "V"; Confirmacao = "v" ->
                writeln("Voltando..."),
                sleep(2),
                criar_aula(MenuPrincipal)
        ;
            writeln("Opçao invalida. Por favor, digite C para confirmar ou V para voltar."),
            writeln("Tente Novamente"),
            sleep(2),
            confirmar_dados(Nome, Horario, Planos_Escolhidos,  MenuPrincipal)
    ).

ler_aula(Nome, MenuPrincipal) :-
    
        atom_concat('BD/aula/', Nome, Temp),
        atom_concat(Temp, '.json', Arquivo),
        open(Arquivo, read, StreamRead),
        json_read_dict(StreamRead, Aula),
        close(StreamRead),
        writeln("Informacoes da Aula: "),
        format('Nome: ~s~n', [Aula.nome]),
        format('Horario: ~s~n', [Aula.horario]),
        writeln('Planos Escolhidos:'),
        imprimir_planos(Aula.planos).
    
      

listar_todas_aulas(Path) :-
    directory_files(Path, Lista_Arquivos),
    processar_arquivos_json(Lista_Arquivos, Path).

processar_arquivos_json([], _).
processar_arquivos_json([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_e_mostrar_aula(Caminho),
        processar_arquivos_json(Arquivos, Diretorio)
    ;   processar_arquivos_json(Arquivos, Diretorio)
    ).

ler_e_mostrar_aula(Arquivo) :-
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Aula),
    close(Stream),
    format('Nome: ~s~n', [Aula.nome]),
    format('Horario: ~s~n', [Aula.horario]),
    writeln('Planos Escolhidos:'),
    imprimir_planos(Aula.planos),
    writeln('').

remover_aula(Nome) :-
    
    (   aula_existe(Nome)
    ->  
        atom_concat('BD/aula/', Nome, Temp),
        atom_concat(Temp, '.json', Arquivo),
        delete_file(Arquivo),
        writeln("Aula removida com sucesso!")
    ;   writeln("Aula nao existe!")
    ).

atualizarAulaPorNome(Nome, NumeroCampo, NovoValor) :-
    
    (   atom_concat('BD/aula/', Nome, Temp),
            atom_concat(Temp, '.json', Arquivo),
            open(Arquivo, read, Stream),
            json_read_dict(Stream, Aula),
            close(Stream),

            (   NumeroCampo = 1 ->
                        AulaAtualizada = Aula.put(horario, NovoValor)
                ;   NumeroCampo = 2 ->
                        AulaAtualizada = Aula.put(planos, NovoValor)
                        
            ),

            writeln('\nAtualizando...'),
            sleep(2),
    

            open(Arquivo, write, StreamWrite),
            json_write(StreamWrite, AulaAtualizada),
            close(StreamWrite),

            writeln('\nAula Atualizada!'),
            sleep(2)
        ).

        
    