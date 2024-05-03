:- module(Aula, [aula/3]).

:- dynamic aula/3.

:- op(600, xfx, cpf).
:- op(600, xfx, senha).
:- op(600, xfx, tipoUsuario).

aula(Nome, Horario, [Planos]) :-
    assertz(aula(Nome, Horario, Planos)).