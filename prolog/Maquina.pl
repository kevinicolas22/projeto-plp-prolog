:- dynamic maquina/3.

:- op(600, xfx, codigoMaquina). 
:- op(600, xfx, nomeMaquina). 
:- op(600, xfx, dataManutencao).

maquina(CodigoMaquina, NomeMaquina, DataManutencao) :- assertz(maquina(CodigoMaquina, NomeMaquina, DataManutencao)).