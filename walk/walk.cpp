
/*
 * file: walk.c
 * desc: Performant random walk with restart (RWR) implementation in C++.
 * auth: TR
 * vers: 0.1.0
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

    // Generate an inverse adjacency list, mapping colunms -> rows
    std::vector<std::vector<std::pair<int, double>>> clist;

    clist.resize( alist.size() );

    // Loop through each row, storing the column -> row associations in the new
    // adjacency list
    for (size_t r = 0; r < alist.size(); r++) {

        auto cmap = alist[r];

        for (size_t c = 0; c < cmap.size(); c++) {
			auto cell = alist[r][c];

            clist[cell.first].push_back( std::make_pair(r, cell.second) );
        }
    }

    // Convert back to the normal adjacency list representation
    AdjacencyList newlist;

    newlist.resize( alist.size() );

    // Begin normalizing each column
    for (size_t c = 0; c < clist.size(); c++) {

        sum = 0.0;

        // Sum the rows
        for (size_t r = 0; r < clist[c].size(); r++) {

            sum += clist[c][r].second;
        }

        // Normalize and convert back to the original adjacency list
        // representation
        for (size_t r = 0; r < clist[c].size(); r++) {

            newlist[clist[c][r].first].push_back( 
                std::make_pair(c, clist[c][r].second / sum)
            );
        }

    }

    return newlist;
}

double *initialProxVector2( int size, int seedSize, int *seed ) {

    double *v = (double *) malloc(size * sizeof(double));

    for (int i = 0; i < size; i++)
		v[i] = 0.0;

	for (int i = 0; i < seedSize; i++) {

		if (seed[i] < 0 || seed[i] >= size)
			seed[i] = 0;

		v[seed[i]] = 1.0 / static_cast<float>(seedSize);
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
        for (auto j = 0U; j < alist[i].size(); j++) {

			auto cell = alist[i][j];

            vnew[i] += cell.second * v[cell.first];
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

	log( VERBOSE, "[+] Generating initial proximity vector" );

    double *p0 = initialProxVector2(size, seedSize, seed);

	log( VERBOSE, "[+] Generating initial proximity vector...");

	double *preVector = initialProxVector2(size, seedSize, seed);

	log( VERBOSE,"[+] Calculating proximity vectory...");

    double *curVector = calculateProxVector(
        size, preVector, p0, normalMatrix, a0, a1
    );

    double threshold = 1.0E-5;

	log( VERBOSE,"[+] Walking the graph...");

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

        log( VERBOSE,"[+] Freeing matrix memory...");

        for (int i = 0; i < size; i++)
            free(normalMatrix[i]);

        free(normalMatrix);
    }

	log( VERBOSE,"[+] Freeing restart vector memory...");

    free(p0);

	log( VERBOSE,"[+] Freeing previous vector memory...");

    free(preVector);

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

	log( VERBOSE,"[+] Generating initial proximity vector...");

    double *p0 = initialProxVector2(size, seedSize, seed);

	log( VERBOSE,"[+] Generating secondary proximity vector...");

	double *preVector = initialProxVector2(size, seedSize, seed);

	log( VERBOSE,"[+] Calculating proximity vectory...");

    double *curVector = calculateProxVectorAList(
        size, preVector, p0, alist, a0, a1
    );

    double threshold = 1.0E-8;

	log( VERBOSE,"[+] Walking the graph...");

    while (calculateConvergence(size, preVector, curVector) > threshold) {

        free(preVector);

        preVector = curVector;
        curVector = calculateProxVectorAList(
            size, preVector, p0, alist, a0, a1
        );
    }

	log( VERBOSE,"[+] Freeing restart vector memory...");

    free(p0);

	log( VERBOSE,"[+] Freeing previous vector memory...");

    free(preVector);

    return curVector;
}

