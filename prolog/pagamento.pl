:- module(Pagamento, [pagamento/2]).

:- dynamic plano/7.

:- op(600, xfx, valor).
:- op(600, xfx, plano).


pagamento(valor,plano):-
    assertz(pagamento(valor,plano)).