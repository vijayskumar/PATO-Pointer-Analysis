//#include <stdio.h>

int main() {
	int a, a1, a2, a3;
	int *p, *p1, *p2, *p3, *p4;
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

//	printf("%d\n", **pp);

}
// p -> a; 
