:- module(Aluno, [aluno/7]).

:- dynamic aluno/7.

:- op(600, xfx, matricula).
:- op(600, xfx, nomeAluno).
:- op(600, xfx, cpfAluno).
:- op(600, xfx, enderecoAluno).
:- op(600, xfx, contatoAluno).
:- op(600, xfx, planoAluno).
:- op(600, xfx, treinos).
:- op(600, xfx, emDia).
:- op(600, xfx, senhaAluno).
:- op(600, xfx, aulasAluno).
:- op(600, xfx, saldo).


aluno(matricula,nomeAluno,cpfAluno,enderecoAluno,contatoAluno,planoAluno,treinos,emDia,senhaAluno,aulasAluno,saldo):-
    assertz(aluno(matricula,nomeAluno,cpfAluno,enderecoAluno,contatoAluno,planoAluno,treinos,emDia,senhaAluno,aulasAluno,saldo)).




