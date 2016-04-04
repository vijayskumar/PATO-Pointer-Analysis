#include <stdio.h>
#include <stdlib.h>
int main() {
	int a, a1, a2, a3;
	int *p, *p1, *p2, *p3, *p4, *p5;
	int **pp;
	
	a = 1; a1 = 2; a2 = 3, a3 = 4;
	
	p = &a;
	p1 = p;
	p1 = &a1; 
	p2 = &a2;
	p4 = &a3;

	pp = &p1;
	pp = &p2;

	p3 = *pp;
	*pp = p4;
	
	p5 = (int *)malloc(3 * sizeof(int));
	
	p5[2] = 5;

	//printf("%d, %d\n", **pp, p5[2]);

	free(p5);

}
/*
p {a}
p1 {a, a1, a3}
p2 {a2, a3}
p4 {a3}
pp {p1, p2}
p3 {a, a1, a2, a3}
p5 {heap}
*/
