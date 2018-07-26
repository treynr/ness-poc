
/*
 * file: walk.c
 * auth: TR
 * vers: 0.1.0
 * desc: "Performant" random walk with restart (RWR) implementation in C/C++.
 * 		  For use with Haskell's FFI
 */

#include <cmath>
#include <cstdio>
#include <cstdlib>
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
        
    m = (double **) malloc(n * sizeof(double *));

    for (int r = 0; r < n; r++)
        m[r] = (double *) malloc(n * sizeof(double));

    for (int r = 0; r < n; r++)
        for (int c = 0; c < n; c++)
            m[r][c] = 0.0;

    return m;
}

/*
 * Remember to deallocate.
 */
//double **normalizeColumnsOld( int size, double **m ) {
//
//    double **mnew = allocateNxN(size);
//    double **temp = allocateNxN(size);
//
//    // store values in column major order
//    for (int r = 0; r < size; r++) {
//        for (int c = 0; c < size; c++) {
//            temp[c][r] = m[r][c];
//        }
//    }
//
//    for (int c = 0; c < size; c++) {
//        temp[c] = l1Normalize(size, temp[c]);
//    }
//
//    for (int r = 0; r < size; r++) {
//        for (int c = 0; c < size; c++) {
//
//            mnew[c][r] = temp[r][c];
//        }
//    }
//
//    for (int i = 0; i < size; i++)
//        free(temp[i]);
//
//    free(temp);
//
//    return mnew;
//}

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

double **normalizeColumnsInPlace( int size, double **m ) {

	double sum = 0.0;

    for (int c = 0; c < size; c++) {

		sum = 0.0;

        for (int r = 0; r < size; r++)
			sum += fabs(m[r][c]);

        for (int r = 0; r < size; r++) {

			if (sum == 0.0)
				m[r][c] = 0.0;
			else
				m[r][c] = m[r][c] / sum;
		}
    }

    return m;
}

AdjacencyList normalizeColumnsAList( int size, AdjacencyList alist ) {

	double sum = 0.0;

    for (int c = 0; c < size; c++) {

		sum = 0.0;

        for (int r = 0; r < size; r++) {

            auto mit = alist[r].find(c);

            if (mit == alist[r].end())
                continue;

			sum += fabs(mit->second);
        }

        for (int r = 0; r < size; r++) {
            auto cit = alist[r].find(c);

            if (cit == alist[r].end())
                continue;

            if (sum == 0.0)
                cit->second = 0.0;
            else
                cit->second = cit->second / sum;
        //for (auto vit = alist.begin(); vit != alist.end(); vit++) {
            //for (auto cit = alist[r].begin(); cit != alist[r].end(); cit++) {
            ////for (auto cit = vit->begin(); cit != vit->end(); cit++) {
            //    //if (sum == 0.0)
            //    //    cit->second = 0.0;
            //    //else
            //    //    cit->second = cit->second / sum;
            //    if (sum == 0.0)
            //        alist[r][cit->first] = 0.0;
            //    else
            //        alist[r][cit->first] = cit->second / sum;
            //    //std::cout << cit->second << std::endl;
            //}
            //for (auto cit = alist[r].begin(); cit != alist[r].end(); cit++) {
            //    std::cout << cit->first << ", " << cit->second << std::endl;
            //}
		}
    }
    //for (auto i = 0; i < size; i++) {
    //    std::cout << i << ": ";
    //    for (auto mit = alist[i].begin(); mit != alist[i].end(); mit++) {
    //        std::cout << "(" << mit->first << ", " << mit->second;
    //        std::cout << ") ";
    //    }
    //    std::cout << std::endl;
    //}

    return alist;
}

double *initialProxVector2( int size, int seedSize, int *seed ) {

    double *v = (double *) malloc(size * sizeof(double));

    for (int i = 0; i < size; i++)
		v[i] = 0.0;

	for (int i = 0; i < seedSize; i++) {

		if (seed[i] < 0 || seed[i] >= size)
			seed[i] = 0;

		v[seed[i]] = 1.0 / (float)seedSize;
	}

    return v;
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

double *multiplyMatrixByVectorAList( int msize, int vsize, AdjacencyList alist, double *v ) {

    double *vnew = (double *) malloc(vsize * sizeof(double));

    for (int i = 0; i < vsize; i++)
        vnew[i] = 0.0;

    for (int i = 0; i < msize; i++) {
        for (auto jit = alist[i].begin(); jit != alist[i].end(); jit++) {

            vnew[i] += jit->second * v[jit->first];
            //std::cout << jit->first << ", " << jit->second << std::endl;
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

    for (int i = 0; i < size; i++)
        res[i] += eps[i];

    free(mxv);
    free(eps);

    return res;
};

double *calculateProxVectorAList( 
    int size, 
    double *pv, 
    double *rv, 
    AdjacencyList alist, 
    double alpha0, 
    double alpha1
) {

    double *res = multiplyVectorByScalar(size, rv, alpha0);
    double *mxv = multiplyMatrixByVectorAList(size, size, alist, pv);
    //printNZElements(size, mxv);
    double *eps = multiplyVectorByScalar(size, mxv, alpha1);

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

    return c;
}

double *randomWalkVector(
    int size, 
    int seedSize, 
    int *seed, 
    double *v, 
    double alpha 
) {

	double **m = allocateNxN(size);

	printf("[C] Converting vector to matrix...\n");

	for (int r = 0; r < size; r++)
		for (int c = 0; c < size; c++)
			m[r][c] = v[size * r + c];

	return randomWalkMatrix(size, seedSize, seed, m, alpha, 1.0 - alpha, false);
}

double *randomWalkMatrix(
    int size, 
    int seedSize, 
    int *seed, 
    double **m, 
    double a0, 
    double a1,
    bool deallocateMatrix = false
) {

    double **normalMatrix = m;

	printf("[C] Generating initial proximity vector...\n");

    double *p0 = initialProxVector2(size, seedSize, seed);

	printf("[C] Generating initial proximity vector...\n");

	double *preVector = initialProxVector2(size, seedSize, seed);

	printf("[C] Calculating proximity vectory...\n");

    double *curVector = calculateProxVector(
        size, preVector, p0, normalMatrix, a0, a1
    );

    double threshold = 1.0E-5;

	printf("[C] Walking the graph...\n");

    while (calculateConvergence(size, preVector, curVector) > threshold) {

        free(preVector);

        preVector = curVector;
        curVector = calculateProxVector(
            size, preVector, p0, normalMatrix, a0, a1
        );
    }

    // If we're calling this function from Haskell, this needs to be done
    // otherwise we probably don't want to do this yet
    if (deallocateMatrix) {

        printf("[C] Freeing matrix memory...\n");

        for (int i = 0; i < size; i++)
            free(normalMatrix[i]);

        printf("[C] Freeing matrix memory...\n");

        free(normalMatrix);
    }

	printf("[C] Freeing restart vector memory...\n");

    free(p0);

	printf("[C] Freeing previous vector memory...\n");

    free(preVector);

	printf("[C] Returning...\n");

    return curVector;
}

double *randomWalkAList(
    int size, 
    int seedSize, 
    int *seed, 
    AdjacencyList alist,
    double a0, 
    double a1
) {

	printf("[C] Generating initial proximity vector...\n");

    double *p0 = initialProxVector2(size, seedSize, seed);

	printf("[C] Generating secondary proximity vector...\n");

	double *preVector = initialProxVector2(size, seedSize, seed);

	printf("[C] Calculating proximity vectory...\n");

    double *curVector = calculateProxVectorAList(
        size, preVector, p0, alist, a0, a1
    );

    double threshold = 1.0E-5;

	printf("[C] Walking the graph...\n");
    //std::cout << size << std::endl;
    //printNZElements(size, curVector);

    while (calculateConvergence(size, preVector, curVector) > threshold) {

        free(preVector);

        preVector = curVector;
        curVector = calculateProxVectorAList(
            size, preVector, p0, alist, a0, a1
        );
    }

	printf("[C] Freeing restart vector memory...\n");

    free(p0);

	printf("[C] Freeing previous vector memory...\n");

    free(preVector);

	printf("[C] Returning...\n");

    return curVector;
}
