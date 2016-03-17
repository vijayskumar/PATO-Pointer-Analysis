#include <stdio.h>
#include <stdlib.h>
#include "header.h"

struct ts {
	int a;
	float *p;
	char c;
};

typedef struct ts T;

int foo(int a);

float * id(float *f) {
	return f;
}

int plus(int a, int b) {
	return a + b;
}

int plus2(int *a, int *b) {
	return (*a + *b);
}

int main() {
	int c;
	float f = 3.8, *fp;
	int *cp, *cp2;
	float **fpp;
	T st, *stp;

	float* parr[3] = { [1] = &f }; // addr !

	float* pl = (float *)malloc(5 * sizeof(float)); // malloc
	float* pl2;
	pl2 = (float *)malloc(3 * sizeof(float)); // malloc

	for (int i = 0; i < 5; ++i)
	{
		pl[i] = i;
	}
	
	c = 24;
	st.a = c; 

	cp = &c; // addr
	*cp += 3;

	cp2 = cp; // copy
	
	stp = &st; // addr
	
	stp->p = &f; // store []
	fpp = &(stp->p); // addr []

	f += *parr[1];
	
	c = plus(stp->a, (int)(*(stp->p))); // call
	c += glb;

	fp = stp->p; // load

	*fpp = fp; // store

	fp = *fpp; // load
	
	printf("%d %f %d\n", stp->a, *(stp->p), c);
	printf("%f\n", **fpp);

	for (int i = 0; i < 5; ++i)
	{
		printf("%f\n", pl[i]);
	}

	free(pl);
	free(pl2);
	
	return c;	
	
}
	
	

