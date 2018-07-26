
/*
 * file: main.cpp
 * auth: TR
 * vers: 0.1.0
 * desc: "Performant" random walk with restart (RWR) implementation in C/C++. 
 *       For use with Haskell's FFI
 */

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <map>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <utility>
#include "walk.h"

typedef std::unordered_map<std::string, int> EntityMap;
typedef std::unordered_map<int, std::string> IndexMap;

// A test graph whose RWR values are known
double graph[6][6] =  { {0.0, 1.0, 0.0, 1.0, 0.0, 1.0},
                        {1.0, 0.0, 1.0, 1.0, 0.0, 0.0},
                        {1.0, 1.0, 0.0, 0.0, 0.0, 0.0},
                        {0.0, 1.0, 0.0, 0.0, 1.0, 0.0},
                        {0.0, 1.0, 0.0, 0.0, 0.0, 0.0},
                        {0.0, 1.0, 0.0, 1.0, 0.0, 0.0} };
// Program arguments
struct Arguments {

    // Filepath to the graph in adjacency matrix form
    std::string s_graph = "";

    // Filepath to the Entity-Index mapping
    std::string s_map = "";

    // Filepath to the list of seeds
    std::string s_seeds = "";

    // Filepath to the output file
    std::string s_output = "";

    // Restart parameter
    double s_restart = 0.15;

    // Remove gene set nodes from the output
    bool s_nsets = false;

    // Remove homology nodes from the output
    bool s_nhomology = false;

    // Use optimized adjacency list representation
    bool s_alist = false;

    // Verbose output
    bool s_verbosity = false;
};

void log( bool verbosity, const char *msg ) {

    if (verbosity)
        std::cout << msg << std::endl;
}

/**
  * Prints program help and usage.
  */
void printHelp( char **argv ) {

    std::cout << "usage: " << argv[0] << " [options] ";
    std::cout << "<graph> <entity-index-map> <seeds> <output>" << std::endl;
    std::cout << "    -a --alist: use an adjacency list graph representation";
    std::cout << std::endl;
    std::cout << "    -r --restart: restart parameter";
    std::cout << std::endl;
    std::cout << "    --no-sets: remove gene set nodes from the output";
    std::cout << std::endl;
    std::cout << "    --no-homology: remove homology nodes from the output";
    std::cout << std::endl;
    std::cout << "    -h --help: print help";
    std::cout << std::endl;
    std::cout << "    -v --verbose: clutter your screen with output";
    std::cout << std::endl;
    std::cout << std::endl;
    std::cout << "arguments <graph>, <entity-index-map>, <seeds>, and ";
    std::cout << "<output-file> are required";
    std::cout << std::endl;
}

/**
  * Parses program arguments.
  */
Arguments parseArguments( int argc, char **argv ) {

    Arguments args;

    for (int i = 1; i < argc; i++) {

        std::string arg = argv[i];

        if (arg == "-r" || arg == "--restart") {

            // Move to the next argument to get the column #
            args.s_restart = (double) strtol(argv[++i], NULL, 10);

        } else if (arg == "--no-sets") {

            args.s_nsets = true;

        } else if (arg == "--no-homology") {

            args.s_nhomology = true;

        } else if (arg == "-v" || arg == "--verbose") {

            args.s_verbosity = true;

        } else if (arg == "-a" || arg == "--alist") {

            args.s_alist = true;

        // Otherwise we just assume it's an argument
        } else {

            if (args.s_graph.empty())
                args.s_graph = argv[i];

            else if (args.s_map.empty())
                args.s_map = argv[i];

            else if (args.s_seeds.empty())
                args.s_seeds = argv[i];

            else if (args.s_output.empty())
                args.s_output = argv[i];
        }
    }

    return args;
}

/**
  * Splits a string using a given delimiter string.
  *
  * arguments
  *     s:      string to split
  *     delim:  delimiter to use while splitting
  *
  * returns
  *     a vector of strings
  */
std::vector<std::string> split( std::string s, std::string delim ) {

    std::vector<std::string> vs;
    std::string token;
    size_t pos = 0;

    while ((pos = s.find(delim)) != std::string::npos) {

        std::string token = s.substr( 0, pos );

        vs.push_back( token );

        s.erase( 0, pos + delim.length() );
    }

    vs.push_back( s );

    return vs;
}

/**
  * Splits a string using a given delimiter character. Unlike normal split, the
  * delimiter must be a character.
  *
  * arguments
  *     s:      string to split
  *     delim:  delimiter to use while splitting
  *
  * returns
  *     a vector of strings
  */
std::vector<std::string> fastSplit( std::string s, char delim ) {

    std::vector<std::string> vs;
    std::string token;

    for (auto i = 0; i < s.length(); i++) {

        if (s[i] == delim) {

            vs.push_back( token );

            token.clear();

        } else {
            token += s[i];
        }
    };

    vs.push_back( token );

    return vs;
}

double **processGraphFile( std::string fp, int &matrixSize ) {

    std::ifstream   file( fp );
    std::string     line;

    if (!file)
        return NULL;

    // The first line contains the size of the NxN matrix
    std::getline(file, line);

    if (line.empty())
        return NULL;

    std::cout << "allocating memory" << std::endl;

    int lineCount = 0;
    int size = static_cast<int>( strtol(line.c_str(), NULL, 10) );
    double **matrix = allocateNxN( size );

    std::cout << "allocated memory" << std::endl;

    while (std::getline(file, line)) {

        //std::cout << "got lines" << std::endl;
        // Shouldn't happen but you never know
        if (line.empty())
            continue;

        // vector of strings representing each column in this row
        //auto columns = split( line, "," );
        auto columns = fastSplit( line, ',' );

        //std::cout << "split lines" << std::endl;

        // The number of columns should be the same as the size of the matrix
        // but we're not checking, so if they're different sizes terrible
        // things will happen
        for (auto i = 0; i < columns.size(); i++) {

            if (columns[i] == "0")
                matrix[lineCount][i] = 0.0;
            else
                matrix[lineCount][i] = strtol( columns[i].c_str(), NULL, 10 );
        }
        //std::cout << "done converting, next line" << std::endl;

        lineCount++;
    }

    matrixSize = size;

    return matrix;
}

AdjacencyList processGraphFileAList( std::string fp, int &matrixSize ) {

    std::ifstream   file( fp );
    std::string     line;

    if (!file)
        return AdjacencyList();

    // The first line contains the size of the NxN matrix
    std::getline(file, line);

    if (line.empty())
        return AdjacencyList();

    AdjacencyList alist;
    int lineCount = 0;
    int size = static_cast<int>( strtol(line.c_str(), NULL, 10) );

    alist.resize( size );

    while (std::getline(file, line)) {

        // Node doesn't have neighbors
        if (line.empty()) {

            alist[lineCount] = std::unordered_map<int, double>();

            continue;
        }

        // vector of strings representing each column in this row
        auto columns = fastSplit( line, ',' );

        if (columns.size() == 0) {

            alist[lineCount] = std::unordered_map<int, double>();

            continue;
        }

        // In this case each column is a node index, so there is an edge
        // between the current node given by lineCount and each node index in
        // the columns variable
        for (auto i = 0; i < columns.size(); i++) {

            int colDex = static_cast<int>( strtol(columns[i].c_str(), NULL, 10) );

            alist[lineCount].insert( std::make_pair(colDex, 1.0) );
            //alist[lineCount].insert( std::pair<int, double>(colDex, 1.0) );
        }

        lineCount++;
    }

    matrixSize = size;

    return alist;
}

EntityMap processEntityFile( std::string fp ) {

    std::ifstream   file( fp );
    std::string     line;
    EntityMap       emap;

    if (!file)
        return EntityMap();

    // The first line contains the size of the number of entity mappings
    // We don't really need this since we just use an unordered map
    std::getline(file, line);

    while (std::getline(file, line)) {

        // Shouldn't happen but you never know
        if (line.empty())
            continue;

        // vector of strings representing each column in this row
        auto columns = fastSplit( line, '\t' );

        // Shouldn't happen but idk
        if (columns.size() < 2)
            continue;

        emap[columns[0]] = static_cast<int>(
            strtol(columns[1].c_str(), NULL, 10) 
        );
    }

    return emap;
};

IndexMap buildIndexMap( EntityMap em ) {

    IndexMap im;

    for (auto it = em.begin(); it != em.end(); it++)
        im[it->second] = it->first;

    return im;
}

std::vector<std::string> processSeedFile( std::string fp ) {

    std::ifstream            file( fp );
    std::string              line;
    std::vector<std::string> seeds;

    if (!file)
        return std::vector<std::string>();

    while (std::getline(file, line)) {

        // Shouldn't happen but you never know
        if (line.empty())
            continue;

        seeds.push_back( line );
    }

    return seeds;
};


int main( int argc, char **argv ) {


    if (argc < 5) {

        printHelp( argv );

        return 1;
    }

    Arguments args = parseArguments( argc, argv );

    log( args.s_verbosity, "[+] Parsing graph file..." );

    int size = 0;
    double **m = NULL;
    AdjacencyList alist;

    if (args.s_alist) {

        alist = processGraphFileAList( args.s_graph, size );

        if (size == 0 || alist.empty()) {

            std::cout << "[!] There was an error processing the graph file";
            std::cout << " or it was empty";
            std::cout << std::endl;

            return 1;
        }

    } else {

        m = processGraphFile( args.s_graph, size );

        if (size == 0 || m == NULL) {

            std::cout << "[!] There was an error processing the graph file";
            std::cout << " or it was empty";
            std::cout << std::endl;

            return 1;
        }
    }

    log( args.s_verbosity, "[+] Parsing Entity-Index mapping file..." );

    EntityMap emap = processEntityFile( args.s_map );
    IndexMap imap = buildIndexMap( emap );

    log( args.s_verbosity, "[+] Checking for missing nodes..." );

    std::cout << size << std::endl;
    std::cout << alist.size() << std::endl;
    // Add any missing entities to the graph
    if (args.s_alist) {

        int maxIndex = 0;

        for (auto eit = emap.begin(); eit != emap.end(); eit++) {
            if (eit->second > maxIndex)
                maxIndex = eit->second;
        }

        if ((maxIndex + 1) > alist.size() && args.s_verbosity)
            std::cout << "[!] Resizing graph, adding " << (maxIndex - alist.size()) << " nodes" << std::endl;

        if ((maxIndex + 1) > alist.size())
            alist.resize( maxIndex + 1 );

        size = alist.size();
    }
    std::cout << alist.size() << std::endl;

    if (emap.empty()) {

        std::cout << "[!] There was an error processing the Entity map file";
        std::cout << " or it was empty";
        std::cout << std::endl;

        return 1;
    }
    
    log( args.s_verbosity, "[+] Parsing seed file..." );

    std::vector<std::string> seeds = processSeedFile( args.s_seeds );

    if (seeds.empty()) {

        std::cout << "[!] There was an error processing the seed file";
        std::cout << " or it was empty";
        std::cout << std::endl;

        return 1;
    }

    std::vector<int> entDexs;

    for (auto entity : seeds) {

        auto eit = emap.find( entity );

        if (eit == emap.end())
            continue;

        entDexs.push_back( eit->second );
    }

    std::cout << "Processed " << seeds.size() << " seeds" << std::endl;
    std::cout << "Processed " << entDexs.size() << " seeds" << std::endl;

    if (entDexs.empty()) {

        std::cout << "[!] None of the seed entities you provided could be";
        std::cout << " converted into indices.";
        std::cout << std::endl;

        return 1;
    }

    log( args.s_verbosity, "[+] Column normalizing the matrix..." );

    if (args.s_alist)
        alist = normalizeColumnsAList( size, alist );
    else
        m = normalizeColumnsInPlace( size, m );

    /*
    for (auto i = 0; i < alist.size(); i++) {
        std::cout << i << ": ";
        for (auto mit = alist[i].begin(); mit != alist[i].end(); mit++) {
            std::cout << "(" << mit->first << ", " << mit->second;
            std::cout << ") ";
        }
        std::cout << std::endl;
    }
    */

    //return 1;
    // File which will contain the results of our walk
    std::ofstream outFile( args.s_output, std::ofstream::out );

    // TODO: Need to add code to start from multiple seeds at once
    int *seed = (int *) malloc(1 * sizeof(int));

    log( args.s_verbosity, "[+] Starting the walk..." );

    for (int i = 0; i < entDexs.size(); i++) {

        seed[0] = entDexs[i];

        double *vector = NULL;

        std::cout << "Seeds left: " << (entDexs.size() - i) << std::endl;
        std::cout << "The seed is: " << seed[0] << std::endl;
        
        if (args.s_alist) {

            std::cout << "Walking the adjacency list..." << std::endl;
            vector = randomWalkAList( 
                size, 1, seed, alist, args.s_restart, 1.0 - args.s_restart
            );

        } else {

            vector = randomWalkMatrix( 
                size, 1, seed, m, args.s_restart, 1.0 - args.s_restart, false
            );
        }

        // Begin converting result indices to entities and sorting
        std::vector<std::pair<std::string, double>> results;

        for (int i = 0; i < size; i++) {

            auto it = imap.find( i );

            if (it != imap.end()) {

                results.push_back( std::make_pair(it->second, vector[i]) );

            } else {
                std::stringstream s;

                s << "UNKNOWNENTITY:" << i;

                results.push_back( std::make_pair(s.str(), vector[i]) );
            }
        }

        // Filtered results
        std::vector<std::pair<std::string, double>> filtResults;

        // Remove values that are zero
        std::copy_if(
            results.begin(),
            results.end(),
            std::back_inserter(filtResults),
            [](std::pair<std::string, double> a) -> bool { return a.second > 0; }
        );

        if (args.s_nsets) {

            filtResults.erase(
                std::remove_if(
                    filtResults.begin(),
                    filtResults.end(),
                    [](std::pair<std::string, double> a) -> bool { return a.first.substr(0, 2) == "GS"; }
                ),
                filtResults.end()
            );
        }

        if (args.s_nhomology) {

            filtResults.erase(
                std::remove_if(
                    filtResults.begin(),
                    filtResults.end(),
                    [](std::pair<std::string, double> a) -> bool { return a.first.substr(0, 3) == "HOM"; }
                ),
                filtResults.end()
            );
        }

        std::sort(
            filtResults.begin(),
            filtResults.end(),
            [=](std::pair<std::string, double> a, std::pair<std::string, double> b) -> bool { return a.second > b.second; }
        );

        for (auto it = filtResults.begin(); it != filtResults.end(); it++)
            outFile << imap[entDexs[i]] << "\t" << it->first << "\t" << it->second << std::endl;

        free( vector );
    }



	//double **m = allocateNxN(6);

	//m[0][1] = 1.0;
	//m[0][3] = 1.0;
	//m[0][5] = 1.0;
	//m[1][0] = 1.0;
	//m[1][2] = 1.0;
	//m[1][3] = 1.0;
	//m[2][0] = 1.0;
	//m[2][1] = 1.0;
	//m[3][1] = 1.0;
	//m[3][4] = 1.0;
	//m[4][1] = 1.0;
	//m[5][1] = 1.0;
	//m[5][3] = 1.0;


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

	/*
	double *vec = randomWalkMatrix(6, 0, m, 0.15, 1.0 - 0.15);

    for (int r = 0; r < 6; r++)
		printf("%f ", vec[r]);
		*/

	return 0;
}

