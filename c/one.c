#include <stdio.h>
#include <math.h>

int is_prime (int m) {
	int status = 1;
	int i = 3;
	float lim = sqrt(m);
	while ((i <= lim) && status) {
		if (m % i == 0) {
			status = 0;
		} else {
			i += 2;
		}
	}
	return status;
}

long suma_prima (int lim) {
	long suma = 10;
	int i = 7;
	while (i < lim) {
		short status = 1;
		int j = 3;
		while ((j*j <= i) && status) {
			if (i % j == 0) {
				status = 0;
			} else {
				j += 2;
			}
		}
		if (status) {
			suma += i;
		} ;
		i += 2;
	}
	return suma;
}

int main() {
	printf("%ld", suma_prima(2000000));
}








