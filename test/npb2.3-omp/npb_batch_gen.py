import subprocess

testfiles = subprocess.check_output(['ls', '.'])

for X in testfiles.split():
	cmd = 'time ../../bin/ast2db ./{0} -- -I/usr/lib/llvm-3.7/lib/clang/3.7.1/include | ../../bin/csv2ntriples.py > {0}.trp'.format(X)
	subprocess.check_call(cmd, shell=True)
