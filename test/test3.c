#include <stdio.h>
int* f1(int *a, int *b) {
	int *c;
	a = b;
	c = a;
	return c;
}
int* f2(int **apx, int **bpx);
int main() {
	int a, b, c;
	int *pa, *pb, *pc, *pd;
	
	a = 1; b = 2; c = 3;
	
	pa = &a; pb = &b; 
	
	pc = f1(pa, pb);
	
	printf("%d %d\n", *pc, *pa);
	
	pd = f2(&pa, &pb);
	
	printf("%d %d\n", *pd, *pa);
	
	return 0;
}

int* f2(int **ap, int **bp) {
	int *cp, **ip, *dp;
	ap = bp;
	cp = *ap;
	//*dp = *bp; // error in runtime, dp is not bound to any memory, can used to detect address boundary error
	ip = &cp;
	dp = *ip;
	return dp;	
}
/*
f1
f1:a {a, b}
f1:b {b}
f1:c {a, b}
pc {a, b}

f2
pa {a}
pb {b}
ap {pa, pb}
bp {pb}
cp {a, b}
ip {cp}
dp {a, b}
pd {a, b}
*/
