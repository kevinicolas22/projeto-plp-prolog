:- module(Login, [login/3]).

:- dynamic login/3.

:- op(600, xfx, cpf).
:- op(600, xfx, senha).
:- op(600, xfx, tipoUsuario).

login(CPF, Senha, TipoUsuario) :-
    assertz(login(CPF, Senha, TipoUsuario)).