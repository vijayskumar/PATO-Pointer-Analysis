// #include <stdio.h>

struct TS {
	int a;
	int *f1;
	int *f2;
};

int main() {
	int a1, a2, a3;
	int *p1, *p2, *p3;
	struct TS st1, st2, *stp, st3;

	a1 = 1; a2 = 2; a3 = 3; 

	st2 = st1;

	st1.f1 = &a1;

	p1 = st1.f1;

	p2 = &a2;
	
	stp = &st2;

	stp->f2 = p2;

	st3 = *stp;

	p3 = st3.f2;

	st2.f1 = &a3;
	
	p2 = &a3;

	// printf("%d %d\n", *p1, *p3);	
	
	return 0;
}
/*
.f1 {a1, a3}
.f2 {a2, a3}
p1 {a1, a3}
p2 {a2, a3}
stp {st2}
p3 {a2}
*/
