:- module(AvaliacaoFisica, [avaliacao_fisica/7]).

:- dynamic avaliacao_fisica/7.

:- op(600, xfx, cpf).
:- op(600, xfx, dataAvaliacao).
:- op(600, xfx, peso).
:- op(600, xfx, altura).
:- op(600, xfx, idade).
:- op(600, xfx, objetivo).
:- op(600, xfx, matriculaAlunoAv).

avaliacao_fisica(CPF, DataAvaliacao, Peso, Altura, Idade, Objetivo, MatriculaAluno) :-
    assertz(avaliacao_fisica(CPF, DataAvaliacao, Peso, Altura, Idade, Objetivo, MatriculaAluno)).
