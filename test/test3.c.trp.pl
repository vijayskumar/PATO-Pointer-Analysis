:- dynamic heapLoc/2.


:- dynamic stackLoc/2.

stackLoc('tmpUnaryOperator(15:7:15:8)', 'Var(10:2:10:6)').
stackLoc('tmpUnaryOperator(15:16:15:17)', 'Var(10:2:10:9)').
stackLoc('tmpUnaryOperator(33:7:33:8)', 'Var(29:2:29:7)').
stackLoc('tmpUnaryOperator(21:10:21:11)', 'Var(11:2:11:7)').
stackLoc('tmpUnaryOperator(21:15:21:16)', 'Var(11:2:11:12)').

:- dynamic copy/2.

copy('ParmVar(2:9:2:14)', 'ParmVar(2:17:2:22)').
copy('Var(3:2:3:7)', 'ParmVar(2:9:2:14)').
copy('Var(11:2:11:7)', 'tmpUnaryOperator(15:7:15:8)').
copy('Var(11:2:11:12)', 'tmpUnaryOperator(15:16:15:17)').
copy('Var(11:2:11:17)', 'tmpCallExpr(17:7:17:16)').
copy('Var(11:2:11:22)', 'tmpCallExpr(21:7:21:18)').
copy('ParmVar(28:9:28:15)', 'ParmVar(28:19:28:25)').
copy('Var(29:2:29:7)', 'tmpUnaryOperator(31:7:31:8)').
copy('Var(29:2:29:13)', 'tmpUnaryOperator(33:7:33:8)').
copy('Var(29:2:29:18)', 'tmpUnaryOperator(34:7:34:8)').

:- dynamic load/2.

load('tmpUnaryOperator(31:7:31:8)', 'ParmVar(28:9:28:15)').
load('tmpUnaryOperator(34:7:34:8)', 'Var(29:2:29:13)').

:- dynamic fieldLoad/3.


:- dynamic arrayLoad/2.


:- dynamic store/2.


:- dynamic fieldStore/3.


:- dynamic arrayStore/2.


:- dynamic callProc/2.

callProc('CallExpr(17:7:17:16)', 'Function(2:1:7:1)').
callProc('CallExpr(21:7:21:18)', 'Function(28:1:36:1)').

:- dynamic formalArg/3.

formalArg('Function(2:1:7:1)', '0', 'ParmVar(2:9:2:14)').
formalArg('Function(2:1:7:1)', '1', 'ParmVar(2:17:2:22)').
formalArg('Function(8:1:8:29)', '0', 'ParmVar(8:9:8:15)').
formalArg('Function(8:1:8:29)', '1', 'ParmVar(8:20:8:26)').
formalArg('Function(28:1:36:1)', '0', 'ParmVar(28:9:28:15)').
formalArg('Function(28:1:36:1)', '1', 'ParmVar(28:19:28:25)').

:- dynamic actualArg/3.

actualArg('CallExpr(17:7:17:16)', '0', 'Var(11:2:11:7)').
actualArg('CallExpr(17:7:17:16)', '1', 'Var(11:2:11:12)').
actualArg('CallExpr(21:7:21:18)', '0', 'tmpUnaryOperator(21:10:21:11)').
actualArg('CallExpr(21:7:21:18)', '1', 'tmpUnaryOperator(21:15:21:16)').

:- dynamic formalReturn/2.

formalReturn('Function(2:1:7:1)', 'Var(3:2:3:7)').
formalReturn('Function(28:1:36:1)', 'Var(29:2:29:18)').

:- dynamic actualReturn/2.

actualReturn('CallExpr(17:7:17:16)', 'tmpCallExpr(17:7:17:16)').
actualReturn('CallExpr(21:7:21:18)', 'tmpCallExpr(21:7:21:18)').

%%
