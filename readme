NETWORK ENHANCED SEMANTIC SIMILARITY

ABOUT

    Builds a heterogeneous network from functional genomics data sources
    (GeneWeaver). Idk I'll come back to this.

REQUIREMENTS
	
    gcc
	GHC
	Stack

INSTALLATION

    Ensure gcc and Stack are installed. 
    Run the makefile to compile the core random walk code (written in C) and
    the hetergeneous network bulider (written in Haskell).
    The software can be built and linked either dynamically or statically.

    The dynamic option requires the library containing the core walk code
    (libwalk.so) to be in found in a directory on your $LD_LIBRARY_PATH. 
    This is typically the easiest way to build NESS.

        $ make dynamic

    The static option compiles and links everything, including the C library,
    into a single excutable.
    This option also requires the static versions libgmp and glibc 
    (gmp-static and glibc-static packages on CentOS) to be installed on your 
    system.

        $ make static
	
SYNOPSIS

	stuff

ARGUMENTS

    More stuff

OPTIONS

	-h
	--help

		print the help message

	-v
	--verbose

		clutter your screen with program output

