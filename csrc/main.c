
/*
 * file: main.c
 * auth: TR
 * vers: 0.1.0
 * desc: "Performant" random walk with restart (RWR) implementation in C. For
 * use with Haskell's FFI
 */

#include <stdio.h>
#include <stdlib.h>
#include "walk.h"

//double *normalizeColumns( int size, double *m );
//double *multiplyMatrix( int s1, int s2, double *m1, double *m2 );
//double *randomWalk( int size, double *matrix );
//

double graph[6][6] =  { {0.0, 1.0, 0.0, 1.0, 0.0, 1.0},
                        {1.0, 0.0, 1.0, 1.0, 0.0, 0.0},
                        {1.0, 1.0, 0.0, 0.0, 0.0, 0.0},
                        {0.0, 1.0, 0.0, 0.0, 1.0, 0.0},
                        {0.0, 1.0, 0.0, 0.0, 0.0, 0.0},
                        {0.0, 1.0, 0.0, 1.0, 0.0, 0.0} };

int main( int argc, char **argv ) {

	double **m = allocateNxN(6);

	m[0][1] = 1.0;
	m[0][3] = 1.0;
	m[0][5] = 1.0;
	m[1][0] = 1.0;
	m[1][2] = 1.0;
	m[1][3] = 1.0;
	m[2][0] = 1.0;
	m[2][1] = 1.0;
	m[3][1] = 1.0;
	m[3][4] = 1.0;
	m[4][1] = 1.0;
	m[5][1] = 1.0;
	m[5][3] = 1.0;


	//double **nc1 = normalizeColumns(6, m);
	//double **nc2 = normalizeColumns2(6, m);

	//printMatrix(6, nc1);
	//printMatrix(6, nc2);

	//return 0;
	//printMatrix(6, m);
    //for (int r = 0; r < 5; r++) {
    //    for (int c = 0; c < 5; c++) {
	//		printf("%f ", m[r][c]);
	//	}
	//	printf("\n");
	//}

	double *vec = randomWalkMatrix(6, 0, m, 0.15, 1.0 - 0.15);

    for (int r = 0; r < 6; r++)
		printf("%f ", vec[r]);

	return 0;
}

