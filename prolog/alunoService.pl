:- module(alunoService, [atualizaAlunoPelaMat/3,exibe_avaliacao_aluno/1]).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(mainAluno, [menu_aluno/1]).
:- use_module(avaliacaoFisica).

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

