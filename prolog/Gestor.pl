:- module(gestor, [gestor/6]).

:- dynamic gestor/6.

:- op(600, xfx, gestorId).
:- op(600, xfx, nomeG).
:- op(600, xfx, cpfG).
:- op(600, xfx, dataNascimentoG).
:- op(600, xfx, enderecoG).
:- op(600, xfx, telefoneG).

gestor(GestorId, NomeG, CPFG, DataNascimentoG, EnderecoG, TelefoneG) :-
    assertz(gestor(GestorId, NomeG, CPFG, DataNascimentoG, EnderecoG, TelefoneG)).
