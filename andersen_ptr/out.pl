:- dynamic heapLoc/2.

heapLoc('varCallExpr(39:17:39:41)', 'heapCallExpr(39:17:39:41)').
heapLoc('varCallExpr(37:23:37:47)', 'heapCallExpr(37:23:37:47)').
heapLoc('Var(33:2:33:4)', heap('Var(33:2:33:4)')).
heapLoc('Var(35:2:35:30)', heap('Var(35:2:35:30)')).

:- dynamic stackLoc/2.

stackLoc('varUnaryOperator(49:7:49:8)', 'Var(29:2:29:6)').
stackLoc('varUnaryOperator(54:8:54:9)', 'Var(33:2:33:4)').
stackLoc('varUnaryOperator(56:11:56:12)', 'Var(30:2:30:12)').

:- dynamic copy/2.

copy('Var(38:2:38:9)', 'varCallExpr(39:17:39:41)').
copy('Var(31:2:31:7)', 'varUnaryOperator(49:7:49:8)').
copy('Var(31:2:31:12)', 'Var(31:2:31:7)').
copy('Var(33:2:33:9)', 'varUnaryOperator(54:8:54:9)').
copy('Var(29:2:29:6)', 'varCallExpr(61:6:61:35)').
copy('Var(30:2:30:18)', 'varMemberExpr(64:7:64:12)').
copy('Var(30:2:30:18)', 'varUnaryOperator(68:7:68:8)').
copy('Var(30:2:30:23)', 'varArraySubscriptExpr(72:8:72:14)').
copy('Var(37:2:37:47)', 'varCallExpr(37:23:37:47)').

:- dynamic load/2.

load('varUnaryOperator(68:7:68:8)', 'Var(32:2:32:10)').

:- dynamic fieldLoad/3.

fieldLoad('varMemberExpr(64:7:64:12)', 'Var(33:2:33:9)', 'Field(7:2:7:9)').
fieldLoad('varMemberExpr(61:11:61:16)', 'Var(33:2:33:9)', 'Field(6:2:6:6)').
fieldLoad('varMemberExpr(76:23:76:28)', 'Var(33:2:33:9)', 'Field(6:2:6:6)').

:- dynamic arrayLoad/2.

arrayLoad('varArraySubscriptExpr(72:8:72:14)', 'Var(35:2:35:30)').
arrayLoad('varArraySubscriptExpr(81:18:81:22)', 'Var(37:2:37:47)').

:- dynamic store/2.

store('Var(32:2:32:10)', 'Var(30:2:30:18)').

:- dynamic fieldStore/3.

fieldStore('Var(33:2:33:4)', 'Field(6:2:6:6)', 'Var(29:2:29:6)').
fieldStore('Var(33:2:33:9)', 'Field(7:2:7:9)', 'varUnaryOperator(56:11:56:12)').

:- dynamic arrayStore/2.

arrayStore('Var(37:2:37:47)', 'Var(41:7:41:15)').
arrayStore('Var(35:2:35:30)', 'Var(30:2:30:18)').

:- dynamic callProc/2.

callProc('CallExpr(37:23:37:47)', 'Function(466:1:211:58)').
callProc('CallExpr(39:17:39:41)', 'Function(466:1:211:58)').
callProc('CallExpr(61:6:61:35)', 'Function(19:1:21:1)').
callProc('CallExpr(76:2:76:43)', 'Function(362:1:362:56)').
callProc('CallExpr(77:2:77:22)', 'Function(362:1:362:56)').
callProc('CallExpr(81:3:81:23)', 'Function(362:1:362:56)').
callProc('CallExpr(84:2:84:9)', 'Function(483:1:55:54)').
callProc('CallExpr(85:2:85:10)', 'Function(483:1:55:54)').

:- dynamic formalArg/3.

formalArg('Function(13:1:13:14)', '0', 'ParmVar(13:9:13:13)').
formalArg('Function(15:1:17:1)', '0', 'ParmVar(15:12:15:19)').
formalArg('Function(19:1:21:1)', '0', 'ParmVar(19:10:19:14)').
formalArg('Function(19:1:21:1)', '1', 'ParmVar(19:17:19:21)').
formalArg('Function(23:1:26:1)', '0', 'ParmVar(23:11:23:16)').
formalArg('Function(23:1:26:1)', '1', 'ParmVar(23:19:23:24)').

:- dynamic actualArg/3.

actualArg('CallExpr(61:6:61:35)', '0', 'varMemberExpr(61:11:61:16)').
actualArg('CallExpr(76:2:76:43)', '1', 'varMemberExpr(76:23:76:28)').
actualArg('CallExpr(76:2:76:43)', '3', 'Var(29:2:29:6)').
actualArg('CallExpr(81:3:81:23)', '1', 'varArraySubscriptExpr(81:18:81:22)').
actualArg('CallExpr(84:2:84:9)', '0', 'Var(37:2:37:47)').
actualArg('CallExpr(85:2:85:10)', '0', 'Var(38:2:38:9)').

:- dynamic formalReturn/2.

formalReturn('Function(15:1:17:1)', 'ParmVar(15:12:15:19)').
formalReturn('Function(23:1:26:1)', 'Var(24:2:24:18)').
formalReturn('Function(28:1:89:1)', 'Var(29:2:29:6)').

:- dynamic actualReturn/2.

actualReturn('CallExpr(61:6:61:35)', 'varCallExpr(61:6:61:35)').

%%
