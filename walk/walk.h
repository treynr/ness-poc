#ifndef WALK_H__
#define WALK_H__

/*
 * file: walk.c
 * auth: TR
 * vers: 0.1.0
 * desc: "Performant" random walk with restart (RWR) implementation in C. For
 * use with Haskell's FFI
 */

#include <iostream>
#include <unordered_map>
#include <vector>

//typedef std::vector<std::unordered_map<int, double>> AdjacencyList;
typedef std::vector<std::vector<std::pair<int, double>>> AdjacencyList;

void printMatrix( int n, double **m );

double *l1Normalize( int size, double *v );

double **allocateNxN( int n );

double **normalizeColumns( int size, double **m );

double **normalizeColumnsInPlace( int size, double **m );

AdjacencyList normalizeColumnsAList( int size, AdjacencyList alist );
AdjacencyList normalizeColumnsAListNew( int size, AdjacencyList alist );

double *initialProxVector( int size, int seed );

double *multiplyMatrixByVector( int msize, int vsize, double **m, double *v );

double *multiplyVectorByScalar( int size, double *v, double s );

double *calculateProxVector( 
    int size, 
    double *pv, 
    double *rv, 
    double **m, 
    double alpha0, 
    double alpha1
);

double calculateConvergence( int size, double *pv, double *cv );

//double *randomWalkMatrix( int size, int seed, double **m, double a0, double a1 );
double *randomWalkMatrix( int size, int seedSize, int *seed, double **m, double a0, double a1 );
//double *randomWalkVector( int size, int seed, double *v, double a0, double a1 );
double *randomWalkVector( int size, int seedSize, int *seed, double *v, double alpha );
double *randomWalkMatrix(
    int size, 
    int seedSize, 
    int *seed, 
    double **m, 
    double a0, 
    double a1,
    bool deallocateMatrix
);

double *randomWalkAList(
    int size, 
    int seedSize, 
    int *seed, 
    AdjacencyList alist,
    double a0, 
    double a1
);

#endif
