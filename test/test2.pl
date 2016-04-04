:- dynamic heapLoc/2.

heapLoc('tmpCallExpr(6:23:6:47)', 'heapCallExpr(6:23:6:47)').

:- dynamic stackLoc/2.

stackLoc('tmpUnaryOperator(9:7:9:8)', 'Var(4:2:4:6)').
stackLoc('tmpUnaryOperator(9:16:9:17)', 'Var(4:2:4:9)').
stackLoc('tmpUnaryOperator(15:12:15:13)', 'Var(4:2:4:12)').

:- dynamic copy/2.

copy('Var(5:2:5:7)', 'tmpUnaryOperator(9:7:9:8)').
copy('Var(5:2:5:12)', 'tmpUnaryOperator(9:16:9:17)').
copy('Var(5:2:5:17)', 'tmpArraySubscriptExpr(17:7:17:13)').
copy('Var(6:2:6:47)', 'tmpCallExpr(6:23:6:47)').

:- dynamic load/2.


:- dynamic fieldLoad/3.


:- dynamic arrayLoad/2.

arrayLoad('tmpArraySubscriptExpr(17:7:17:13)', 'Var(6:2:6:47)').

:- dynamic store/2.


:- dynamic fieldStore/3.


:- dynamic arrayStore/2.

arrayStore('Var(6:2:6:47)', 'Var(5:2:5:7)').
arrayStore('Var(6:2:6:47)', 'Var(5:2:5:12)').
arrayStore('Var(6:2:6:47)', 'tmpUnaryOperator(15:12:15:13)').

:- dynamic callProc/2.

callProc('CallExpr(6:23:6:47)', 'Function(466:1:211:58)').
callProc('CallExpr(19:2:19:11)', 'Function(483:1:55:54)').

:- dynamic formalArg/3.


:- dynamic actualArg/3.

actualArg('CallExpr(19:2:19:11)', '0', 'Var(6:2:6:47)').

:- dynamic formalReturn/2.


:- dynamic actualReturn/2.


%%
