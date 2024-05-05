:- module(alunoService, [atualizaAlunoPelaMat/3,exibe_avaliacao_aluno/1, carregar_e_exibir_treinos/1, adiciona_solicitacao/2, criar_aluno/0, aluno_existe/1,listar_alunos/1,qnt_alunos_premium/1,qnt_alunos_gold/1,qnt_alunos_light/1]).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(mainAluno, [menu_aluno/1]).
:- use_module(avaliacaoFisica).
:- use_module(treino).

exibe_avaliacao_aluno(Aluno):-
    atom_concat('BD/avaliacao_fisica/', Aluno.cpfAluno, Temp),
    atom_concat(Temp, '.json', Arquivo),
    (exists_file(Arquivo)->
        open(Arquivo, read, Stream),
        json_read_dict(Stream, AvaliacaoFisica),
        close(Stream),
        write('\n\nData da avaliacao: '), format("~s", [AvaliacaoFisica.dataAvaliacao]),
        write('\nPeso: '), format("~s", [AvaliacaoFisica.peso]), write('Kg'),
        write('\nAltura: '), format("~s", [AvaliacaoFisica.dataAvaliacao]), write('m'),
        write('\nIdade: '), format("~s", [AvaliacaoFisica.idade]),
        write('\nObjetivo: '), format("~s", [AvaliacaoFisica.objetivo]);
    write('\n\n- Voce ainda nao realizou nenhuma avaliacao fisica.')
        ).
    

atualizaAlunoPelaMat(Matricula, CampoDeAtualizacao, NovoValor):-
    atom_concat('BD/aluno/', Matricula, Temp),
    atom_concat(Temp, '.json', Arquivo),
    open(Arquivo, read, Stream),
    json_read_dict(Stream, Aluno),
    close(Stream),
    (CampoDeAtualizacao = 12 -> 
        Aluno2= Aluno.put(planoAluno, NovoValor),
        AlunoAtualizado= Aluno2.put(emDia, "false");
    CampoDeAtualizacao = 1 -> AlunoAtualizado = Aluno.put(nomeAluno, NovoValor);
    CampoDeAtualizacao = 2 -> AlunoAtualizado = Aluno.put(enderecoAluno, NovoValor);
    CampoDeAtualizacao = 3 -> AlunoAtualizado = Aluno.put(contatoAluno, NovoValor);
    CampoDeAtualizacao = 4 -> AlunoAtualizado = Aluno.put(senhaAluno, NovoValor);
    CampoDeAtualizacao = 11 -> 
        get_dict(aulasAluno, Aluno, AulasAtuais),
        append(AulasAtuais, [NovoValor], NovasAulas),
        AlunoAtualizado = Aluno.put(aulasAluno,NovasAulas);
    CampoDeAtualizacao = 13 -> 
        get_dict(aulasAluno, Aluno, AulasAtuais),
        delete(AulasAtuais, NovoValor, NovasAulas),
        AlunoAtualizado = Aluno.put(aulasAluno,NovasAulas);
    CampoDeAtualizacao = 14 -> 
        Aluno2= Aluno.put(emDia, NovoValor),
        atom_concat('BD/plano/',Aluno.planoAluno,Temp2),
        atom_concat(Temp2, '.json', Arquivo2),
        open(Arquivo2, read, StreamRead),
        json_read_dict(StreamRead, Plano),
        close(StreamRead),
        atom_number(Aluno.saldo, SaldoAluno),
        atom_number(Plano.valorMensal, ValorPlano),
        NovoSaldo is SaldoAluno - ValorPlano,
        number_codes(NovoSaldo, Codes),
        atom_codes(Atom, Codes),
        atom_string(Atom, NovoSaldoString),
        AlunoAtualizado =Aluno2.put(saldo, NovoSaldoString);
    CampoDeAtualizacao = 15 -> AlunoAtualizado = Aluno.put(saldo, NovoValor)
     ),
    open(Arquivo, write, WriteStream), % Abre o arquivo para escrita
    json_write_dict(WriteStream, AlunoAtualizado), % Escreve o aluno atualizado no arquivo
    close(WriteStream), % Fecha o arquivo
    menu_aluno(AlunoAtualizado).

carregar_e_exibir_treinos(Aluno) :-
    exibir_treinos(Aluno).

% Predicado para exibir os treinos presentes no dicionário JSON
exibir_treinos(Dict) :-
    % Verifica se o dicionário contém a chave "treinos"
    (   get_dict(treinos, Dict, Treinos),
        % Itera sobre cada treino e exibe seus detalhes
        maplist(exibir_treino, Treinos)
    % Se não houver a chave "treinos"
    ;   format("Não há informações de treinos.~n")
    ).

% Predicado para exibir os detalhes de um treino
exibir_treino(Treino) :-
    % Obtém o nome do treino
    format("\n                ~w~n", [Treino.tipo]),
    write('    ============================='),
    exibe_exercicios(Treino.exercicios),
    write('\n').

exibe_exercicios([Exercicio|Resto]):-
    write('\n      '), write(Exercicio),
    exibe_exercicios_aux(Resto).
    
exibe_exercicios_aux([]):- write('\n').
exibe_exercicios_aux([Exercicio|Resto]):-
    write('\n      '), write(Exercicio),
    exibe_exercicios_aux(Resto).

adiciona_solicitacao(Matricula,TipoTreino):-
    open('BD/solicitacoes/solicitacoes.json', read, ReadStream),
    json_read_dict(ReadStream, Dict),
    close(ReadStream),
    adicionar_solicitacao_dict(Dict, Matricula, TipoTreino, NovoDict),
    open('BD/solicitacoes/solicitacoes.json', write, WriteStream),
    json_write_dict(WriteStream, NovoDict),
    close(WriteStream).

adicionar_solicitacao_dict(Dict, Matricula,TipoTreino, NovoDict):-
    (   get_dict(solicitacoes, Dict, Solicitacoes) ->
        NovasSolicitacoes = [json([matricula_aluno=Matricula, tipo_treino=TipoTreino]) | Solicitacoes]
    ;   NovasSolicitacoes = [json([matricula_aluno=Matricula, tipo_treino=TipoTreino])]
    ),
    put_dict(solicitacoes, Dict, NovasSolicitacoes, NovoDict).

to_lower_case_(String, LowerCaseString) :-
    string_lower(String, LowerCaseString).

criar_aluno:-
    writeln('\n----------- CADASTRO / ALUNO -----------'),
    write('\nNome do aluno: '),
    read_line_to_string(user_input,NovoNome),
    is_null(NovoNome, 'Nome'),
    write('\nCPF: '),
    read_line_to_string(user_input,NovoCpf),
    is_null(NovoCpf, 'Cpf'),
    write('\nEndereco do aluno: '),
    read_line_to_string(user_input,NovoEndereco),
    is_null(NovoEndereco, 'endereco'),
    write('\nContato do aluno: '),
    read_line_to_string(user_input,NovoContato),
    write('\nPlano escolhido: '),
    write('\n\n[L] LIGHT    [G] GOLD    [P] PREMIUM\n:'),
    read_line_to_string(user_input,NovoPlano),
    to_lower_case_(NovoPlano, PlanoLower),
    (PlanoLower= "l" -> PlanoAluno = "light";
    PlanoLower = "g" -> PlanoAluno = "gold";
    PlanoLower = "p" -> PlanoAluno= "premium";
    writeln('> Opcao invalida.'),
    sleep(1.5),
    criar_aluno),
    gerar_matricula_unica(Matricula),
    write('\nMatricula:'), write(Matricula),
    write('\nNova senha de acesso do aluno: '),
    read_line_to_string(user_input,NovaSenha),
    is_null(NovaSenha, 'acesso'),
    write('\nRecarga de saldo inicial: '),
    read_line_to_string(user_input,NovoSaldo),
    NovoAluno = aluno{
            matricula: Matricula,
            nomeAluno: NovoNome,
            cpfAluno: NovoCpf,
            enderecoAluno: NovoEndereco,
            contatoAluno: NovoContato,
            planoAluno: PlanoAluno,
            treinos: [],
            emDia: "false",
            senhaAluno: NovaSenha,
            aulasAluno: [],
            saldo: NovoSaldo
    },
    adiciona_aluno(NovoAluno).
    
qnt_alunos_premium(Path) :-
    directory_files(Path, Arquivos),
    processar_aluno_json(Arquivos, Path, "premium",0). 

qnt_alunos_gold(Path) :-
    directory_files(Path, Arquivos),
    processar_aluno_json(Arquivos, Path, "gold",0). 

qnt_alunos_light(Path) :-
    directory_files(Path, Arquivos),
    processar_aluno_json(Arquivos, Path, "light",0). 

processar_aluno_json([], _,_,Quantidade):-
    write(Quantidade).
processar_aluno_json([Arquivo|Arquivos], Diretorio, Plano, Quantidade) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    ( string_concat(_, '.json', Arquivo)  ->  
        open(Caminho, read, Stream), 
        json_read_dict(Stream, Aluno),
        PlanoAluno = Aluno.planoAluno,
        close(Stream),
        (PlanoAluno= Plano ->
            NewQuantidade is Quantidade+1,
            processar_aluno_json(Arquivos, Diretorio, Plano, NewQuantidade);
            processar_aluno_json(Arquivos, Diretorio, Plano, Quantidade)
        );
        processar_aluno_json(Arquivos, Diretorio, Plano, Quantidade)
    ).


listar_alunos(Path) :-
    directory_files(Path, Arquivos), % Obtém todos os arquivos no diretório 'aluno'
    processar_arquivos_json(Arquivos, Path). % Lê cada arquivo de aluno e extrai as informações

processar_arquivos_json([], _).
processar_arquivos_json([Arquivo|Arquivos], Diretorio) :-
    atomic_list_concat([Diretorio, '/', Arquivo], Caminho),
    (   string_concat(_, '.json', Arquivo)
    ->  ler_aluno(Caminho),
        processar_arquivos_json(Arquivos, Diretorio)
    ;   processar_arquivos_json(Arquivos, Diretorio)
    ).
ler_aluno(NomeArquivo) :-
    open(NomeArquivo, read, Stream), 
    json_read_dict(Stream, Aluno), 
    write('\n-Nome: '),writeln(Aluno.nomeAluno),
    write(' Matricula: '), writeln(Aluno.matricula),
    close(Stream). % Fecha o arquivo

adiciona_aluno(Aluno):-
    Matricula = Aluno.matricula,
    atom_concat('BD/aluno/', Matricula, Temp),
    atom_concat(Temp, '.json', Arquivo),
    open(Arquivo, write, StreamWrite),
    json_write(StreamWrite, Aluno),
    close(StreamWrite).
is_null(String, Campo):-
    (String = "" -> write("\n> O "), write(Campo), writeln(' nao pode estar em branco.'), sleep(1.5), criar_aluno ;
    true).
gerar_matricula_unica(Matricula) :-
    repeat, % repete indefinidamente
    gerar_matricula(Matricula), 
    \+ aluno_existe(Matricula). 

gerar_matricula(Matricula) :-
    between(1, 999, Numero), % Gera um número entre 1 e 999
    format(string(Matricula), '~|~`0t~d~3+', [Numero]).

aluno_existe(Matricula):-
    atom_concat('BD/aluno/',Matricula,Temp),
    atom_concat(Temp, '.json', Arquivo),
    exists_file(Arquivo).