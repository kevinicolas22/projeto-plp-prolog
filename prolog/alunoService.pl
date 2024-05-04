:- module(alunoService, [atualizaAlunoPelaMat/3,exibe_avaliacao_aluno/1, carregar_e_exibir_treinos/1]).
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
    format("\n=> TIPO DE TREINO: ~w~n", [Treino.tipo]),
    exibe_exercicios(Treino.exercicios).

exibe_exercicios([Exercicio|Resto]):-
    write('   Exercicios: '), write(Exercicio),
    exibe_exercicios_aux(Resto).
    
exibe_exercicios_aux([]):- write('\n').
exibe_exercicios_aux([Exercicio|Resto]):-
    write('\n               '), write(Exercicio),
    exibe_exercicios_aux(Resto).

