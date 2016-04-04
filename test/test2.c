#include <stdio.h>
#include <stdlib.h>
int main() {
	int a, b, c;
	int *pa, *pb, *pc;
	int **parr = (int **)malloc(3 * sizeof(int *));
	a = 1; b = 2; c = 3;
	
	pa = &a; pb = &b;
	
	parr[0] = pa;
	
	parr[1] = pb;
	
	parr[2] = &c;
	
	pc = parr[2];
	
	free(parr);
	
	return 0;
}
/*
pa {a}
pb {b}
parr {a, b, c}
pc {a, b, c}
*/
