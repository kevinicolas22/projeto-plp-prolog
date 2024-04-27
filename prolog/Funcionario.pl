:- module(funcionario, [funcionario/7]).

:- dynamic funcionario/7.

:- op(600, xfx, funcId).
:- op(600, xfx, nome).
:- op(600, xfx, cpf).
:- op(600, xfx, endereco).
:- op(600, xfx, telefone).
:- op(600, xfx, data_ingresso).
:- op(600, xfx, salario).

funcionario(FuncId, Nome, CPF, Endereco, Telefone, DataIngresso, Salario) :-
    assertz(funcionario(FuncId, Nome, CPF, Endereco, Telefone, DataIngresso, Salario)).


