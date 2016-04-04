:- dynamic heapLoc/2.

heapLoc('Var(12:2:12:12)', 'heapVar(12:2:12:12)').
heapLoc('Var(12:2:12:17)', 'heapVar(12:2:12:17)').
heapLoc('Var(12:2:12:28)', 'heapVar(12:2:12:28)').

:- dynamic stackLoc/2.

stackLoc('tmpUnaryOperator(18:11:18:12)', 'Var(10:2:10:6)').
stackLoc('tmpUnaryOperator(22:7:22:8)', 'Var(10:2:10:10)').
stackLoc('tmpUnaryOperator(24:8:24:9)', 'Var(12:2:12:17)').
stackLoc('tmpUnaryOperator(32:11:32:12)', 'Var(10:2:10:14)').
stackLoc('tmpUnaryOperator(34:7:34:8)', 'Var(10:2:10:14)').

:- dynamic copy/2.

copy('Var(12:2:12:17)', 'Var(12:2:12:12)').
copy('Var(11:2:11:7)', 'tmpMemberExpr(20:7:20:11)').
copy('Var(11:2:11:12)', 'tmpUnaryOperator(22:7:22:8)').
copy('Var(12:2:12:23)', 'tmpUnaryOperator(24:8:24:9)').
copy('Var(12:2:12:28)', 'tmpUnaryOperator(28:8:28:9)').
copy('Var(11:2:11:17)', 'tmpMemberExpr(30:7:30:11)').
copy('Var(11:2:11:12)', 'tmpUnaryOperator(34:7:34:8)').

:- dynamic load/2.

load('tmpUnaryOperator(28:8:28:9)', 'Var(12:2:12:23)').

:- dynamic fieldLoad/3.

fieldLoad('tmpMemberExpr(20:7:20:11)', 'Var(12:2:12:12)', 'Field(5:2:5:7)').
fieldLoad('tmpMemberExpr(30:7:30:11)', 'Var(12:2:12:28)', 'Field(6:2:6:7)').

:- dynamic arrayLoad/2.


:- dynamic store/2.


:- dynamic fieldStore/3.

fieldStore('Var(12:2:12:12)', 'Field(5:2:5:7)', 'tmpUnaryOperator(18:11:18:12)').
fieldStore('Var(12:2:12:23)', 'Field(6:2:6:7)', 'Var(11:2:11:12)').
fieldStore('Var(12:2:12:17)', 'Field(5:2:5:7)', 'tmpUnaryOperator(32:11:32:12)').

:- dynamic arrayStore/2.


:- dynamic callProc/2.


:- dynamic formalArg/3.


:- dynamic actualArg/3.


:- dynamic formalReturn/2.


:- dynamic actualReturn/2.


%%
