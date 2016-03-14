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

int plus(int a, int b) {
	return a + b;
}

int main() {
	int c;
	float f = 3.8;
	int *cp;
	float **fpp;
	T st, *stp;

	float* parr[3] = { [1] = &f };

	float* pl = (float *)malloc(5 * sizeof(float));

	for (int i = 0; i < 5; ++i)
	{
		pl[i] = i;
	}
	
	c = 24;
	st.a = c;

	cp = &c;
	*cp += 3;
	
	stp = &st;
	
	stp->p = &f;
	fpp = &(stp->p);

	f += *parr[1];
	
	c = plus(stp->a, (int)(*(stp->p)));
	c += glb;
	
	printf("%d %f %d\n", stp->a, *(stp->p), c);
	printf("%f\n", **fpp);

	for (int i = 0; i < 5; ++i)
	{
		printf("%f\n", pl[i]);
	}

	free(pl);
	
	return c;	
	
}
	
	

