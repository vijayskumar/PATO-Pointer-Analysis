:- dynamic heapLoc/2.


:- dynamic stackLoc/2.

stackLoc('tmpUnaryOperator(10:6:10:7)', 'Var(4:2:4:6)').
stackLoc('tmpUnaryOperator(12:7:12:8)', 'Var(4:2:4:9)').
stackLoc('tmpUnaryOperator(13:7:13:8)', 'Var(4:2:4:13)').
stackLoc('tmpUnaryOperator(14:7:14:8)', 'Var(4:2:4:17)').
stackLoc('tmpUnaryOperator(16:7:16:8)', 'Var(5:2:5:11)').
stackLoc('tmpUnaryOperator(17:7:17:8)', 'Var(5:2:5:16)').

:- dynamic copy/2.

copy('Var(5:2:5:7)', 'tmpUnaryOperator(10:6:10:7)').
copy('Var(5:2:5:11)', 'Var(5:2:5:7)').
copy('Var(5:2:5:11)', 'tmpUnaryOperator(12:7:12:8)').
copy('Var(5:2:5:16)', 'tmpUnaryOperator(13:7:13:8)').
copy('Var(5:2:5:26)', 'tmpUnaryOperator(14:7:14:8)').
copy('Var(6:2:6:8)', 'tmpUnaryOperator(16:7:16:8)').
copy('Var(6:2:6:8)', 'tmpUnaryOperator(17:7:17:8)').
copy('Var(5:2:5:21)', 'tmpUnaryOperator(19:7:19:8)').

:- dynamic load/2.

load('tmpUnaryOperator(19:7:19:8)', 'Var(6:2:6:8)').

:- dynamic fieldLoad/3.


:- dynamic arrayLoad/2.


:- dynamic store/2.

store('Var(6:2:6:8)', 'Var(5:2:5:26)').

:- dynamic fieldStore/3.


:- dynamic arrayStore/2.


:- dynamic callProc/2.


:- dynamic formalArg/3.


:- dynamic actualArg/3.


:- dynamic formalReturn/2.


:- dynamic actualReturn/2.


%%
