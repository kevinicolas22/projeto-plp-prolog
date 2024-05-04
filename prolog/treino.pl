:- module(Treino, [treino/2]).

:- dynamic plano/7.

:- op(600, xfx, tipo).
:- op(600, xfx, exercicios).


treino(tipo, exercicios):-
    assertz(treino(tipo, exercicios)).