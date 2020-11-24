
#include <stdio.h>

int main (int argc, char *argv[]) {
	// Vypocet Eulerova cisla pomoci vztahu
	// e ~ (1 + 1/n)^n
	// n = 2^k
	//
	// Mocninu vypocitame tak, ze hodnotu x = 1 + 1/n
	// k-krat umocnime na druhou. Protoze zpocatku jsou hodnoty
	// x jen o malo vetsi nez 1, tak je reprezentujeme takto:
	// x = 1 + a
	const int k = 66;

	// Vypocet x = 1 + 1/2^k
	long double a = 1;

	for (int i = 0; i < k; i++)
		a /= 2;
	
	// Vypocet x = x^(2^k)
	for (int i = 0; i < k; i++)
		a = 2*a + a*a;   // x^2 = (1 + a)^2 = 1 + (2a + a^2)

	// Tisk Eulerova cisla
	const long double e = 1.0 + a;

	printf ("e = %.18Lf\n", e);

	return 0;
}

