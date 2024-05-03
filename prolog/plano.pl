:- module(Plano, [plano/7]).

:- dynamic plano/7.

:- op(600, xfx, tipo).
:- op(600, xfx, valorMensal).
:- op(600, xfx, horaEntradaMaxima).
:- op(600, xfx, horaEntradaMinima).

plano(tipo,valorMensal,horaEntradaMaxima,horaEntradaMinima):-
    assertz(plano(tipo,valorMensal,horaEntradaMaxima,horaEntradaMinima)).