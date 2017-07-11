
/*
 * file: walk.c
 * auth: TR
 * vers: 0.1.0
 * desc: "Performant" random walk with restart (RWR) implementation in C. For
 * 		 use with Haskell's FFI
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "walk.h"

void appendFile( char *f, char *s ) {

	FILE *fp = fopen(f, "a");

	fprintf(fp, s);
	fprintf(fp, "\n");

	fclose(fp);
}

void printMatrix( int n, double **m ) {

    for (int r = 0; r < n; r++) {
        for (int c = 0; c < n; c++) {
            printf("%f ", m[r][c]);
        }
        printf("\n");
    }

    printf("\n");
}

static inline double getElement( int n, int r, int c, double *m ) {

	return m[n * r + c];
}

/*
 * Remember to deallocate.
 */
double *l1Normalize( int size, double *v ) {

	double sum = 0.0;
    //double *vnew = (double *) malloc(size * sizeof(double));

	for (int i = 0; i < size; i++)
		sum += fabs(v[i]);

	for (int i = 0; i < size; i++)
        v[i] = v[i] / sum;
        //vnew[i] = v[i] / sum;

        
    return v;
}

double **allocateNxN( int n ) {

    double **m;
        
    m = malloc(n * sizeof(double *));

    for (int r = 0; r < n; r++)
        m[r] = malloc(n * sizeof(double));

    for (int r = 0; r < n; r++)
        for (int c = 0; c < n; c++)
            m[r][c] = 0.0;

    return m;
}

/*
 * Remember to deallocate.
 */
double **normalizeColumnsOld( int size, double **m ) {

    double **mnew = allocateNxN(size);
    double **temp = allocateNxN(size);

    // store values in column major order
    for (int r = 0; r < size; r++) {
        for (int c = 0; c < size; c++) {
            temp[c][r] = m[r][c];
        }
    }

    for (int c = 0; c < size; c++) {
        temp[c] = l1Normalize(size, temp[c]);
    }

    for (int r = 0; r < size; r++) {
        for (int c = 0; c < size; c++) {

            mnew[c][r] = temp[r][c];
        }
    }

    for (int i = 0; i < size; i++)
        free(temp[i]);

    free(temp);

    return mnew;
}

double **normalizeColumns( int size, double **m ) {

    double **mnew = allocateNxN(size);
	double sum = 0.0;
    //double **temp = allocateNxN(size);

    for (int c = 0; c < size; c++) {

		sum = 0.0;

        for (int r = 0; r < size; r++) {

            mnew[r][c] = m[r][c];
			sum += fabs(m[r][c]);
        }

        for (int r = 0; r < size; r++) {

			if (sum == 0.0)
				mnew[r][c] = 0.0;
			else
				mnew[r][c] = mnew[r][c] / sum;
		}
    }

    return mnew;
}

double *initialProxVector( int size, int seed ) {

    double *v = (double *) malloc(size * sizeof(double));

    if (seed < 0 || seed >= size)
        seed = 0;

    for (int i = 0; i < size; i++) {

        if (i == seed)
            v[i] = 1.0;
        else
            v[i] = 0.0;
    }

    return v;
}

/*
 * Naive implementation cause I'm too lazy to look at the ATLAS API which is
 * probably stupid.
 */
double *multiplyMatrixByVector( int msize, int vsize, double **m, double *v ) {

    double *vnew = (double *) malloc(vsize * sizeof(double));

    for (int i = 0; i < vsize; i++)
        vnew[i] = 0.0;

    for (int i = 0; i < msize; i++) {
        for (int j = 0; j < vsize; j++) {

            vnew[i] += m[i][j] * v[j];
        }
    }

    return vnew;
}

/*
 * Naive implementation cause I'm too lazy to look at the ATLAS API which is
 * probably stupid.
 */
double *multiplyVectorByScalar( int size, double *v, double s ) {

    double *vnew = (double *) malloc(size * sizeof(double));

    for (int i = 0; i < size; i++)
        vnew[i] = v[i] * s;

    return vnew;
}

void printNZElements( int size, double *v) {

    for (int i = 0; i < size; i++)
		if (v[i] > 0.0)
			printf("%.8f ", v[i]);
	printf("\n");
}

double *calculateProxVector( 
    int size, 
    double *pv, 
    double *rv, 
    double **m, 
    double alpha0, 
    double alpha1
) {

    double *res = multiplyVectorByScalar(size, rv, alpha0);
    double *mxv = multiplyMatrixByVector(size, size, m, pv);
    double *eps = multiplyVectorByScalar(size, mxv, alpha1);

	//printf("res\n");
	//printNZElements(size, res);
	//printf("mxv\n");
	//printNZElements(size, mxv);
	//printf("eps\n");
	//printNZElements(size, eps);
	//printf("--\n");

    for (int i = 0; i < size; i++)
        res[i] += eps[i];

    free(mxv);
    free(eps);

    return res;
};

double calculateConvergence( int size, double *pv, double *cv ) {

    double c = 0.0;

    for (int i = 0; i < size; i++)
        c += fabs(cv[i] - pv[i]);
        //c += fabs(pv[i] - cv[i]);

    return c;
}

double *randomWalkVector( int size, int seed, double *v, double a0, double a1 ) {

	double **m = allocateNxN(size);

	printf("[C] Converting vector to matrix...\n");

	//FILE *fp = fopen("/projects/chesler-lab/ffi-walker.txt", "a");

	//for (int r = 0; r < 20; r++) {
	//	for (int c = 0; c < 50; c++) {
	//		fprintf(fp, "%f ", v[size * r + c]);
	//	}
	//	fprintf(fp, "\n");
	//}
	//fprintf(fp, "\n");
	//fprintf(fp, "\n");
	//fclose(fp);

	for (int r = 0; r < size; r++)
		for (int c = 0; c < size; c++)
			m[r][c] = v[size * r + c];


	return randomWalkMatrix(size, seed, m, a0, a1);
}

double *randomWalkMatrix( int size, int seed, double **m, double a0, double a1 ) {

    // deallocate
	//printf("[C] Column normalizing matrix...\n");
	
    //double **normalMatrix = normalizeColumns(size, m);
    double **normalMatrix = m;

	//FILE *fp = fopen("/projects/chesler-lab/ffi-matrix-before.txt", "w");
	//for (int c = 0; c < size; c++)
	//	for (int r = 0; r < size; r++)
	//		fprintf(fp, "%.12f\n", normalMatrix[r][c]);
	//fprintf(fp, "\n");
	//fclose(fp);

	printf("[C] Generating initial proximity vector...\n");

    double *p0 = initialProxVector(size, seed);

	printf("[C] Generating initial proximity vector...\n");

    double *preVector = initialProxVector(size, seed);

	printf("[C] Calculating proximity vectory...\n");

    double *curVector = calculateProxVector(size, preVector, p0, normalMatrix, a0, a1);
    double threshold = 1.0E-5;

	//FILE *fp = fopen("/projects/chesler-lab/ffi-1st-vector.txt", "w");
	//for (int i = 0; i < size; i++)
	//	fprintf(fp, "%.12f\n", curVector[i]);
	//fprintf(fp, "\n");
	//fclose(fp);

	printf("[C] Walking the graph...\n");

    //return curVector;

	int ii = 0;

    while (calculateConvergence(size, preVector, curVector) > threshold) {

		//if (ii > 1)
		//	break;

		//printf("[C] Convergence value = %.8f...\n", calculateConvergence(size, preVector, curVector));
		//printNZElements(size, preVector);
		//printNZElements(size, curVector);

        free(preVector);

        preVector = curVector;
        curVector = calculateProxVector(size, preVector, p0, normalMatrix, a0, a1);

		//ii++;
    }

	printf("[C] Freeing matrix memory...\n");

    for (int i = 0; i < size; i++)
        free(normalMatrix[i]);

	printf("[C] Freeing matrix memory...\n");

    free(normalMatrix);

	printf("[C] Freeing restart vector memory...\n");

    free(p0);

	printf("[C] Freeing previous vector memory...\n");

    free(preVector);

	printf("[C] Returning...\n");

	//fp = fopen("/projects/chesler-lab/ffi-walker.txt", "w");
	//for (int i = 0; i < size; i++)
	//	fprintf(fp, "%.12f\n", curVector[i]);
	//fprintf(fp, "\n");
	//fclose(fp);

    return curVector;
}

