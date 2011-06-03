#include <stdio.h>

int magoo(int k) {
	return k * k + 2 * k + 1;
	
}

int k = 0;

for(; k < 10; k ++) {

printf("magoo:%i\n", magoo(k));
}
