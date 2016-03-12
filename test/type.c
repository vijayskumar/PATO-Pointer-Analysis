#include <stdio.h>
#include "header.h"

struct ts {
	int a;
	float *p;
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
	
	c = 24;
	st.a = c;

	cp = &c;
	*cp += 3;
	
	stp = &st;
	
	stp->p = &f;
	fpp = &(stp->p);
	
	c = plus(stp->a, (int)(*(stp->p)));
	
	printf("%d %f %d\n", stp->a, *(stp->p), c);
	printf("%f\n", **fpp);
	
	return c;	
	
}
	
	

