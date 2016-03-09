//#include <stdio.h>

void foo(int* a, int *b) {
  if (a[0] > 1) {
    b[0] = 2;
  }
}

void bar(float x, float y); // just a declaration

int main() {
	int c;
	c = 3;
	int d[3] = {2, c, 4};
	c = 20;
	foo(d, &d[1]);
	
	//printf("%d\n", d[1]);
	
	return 0;
}
