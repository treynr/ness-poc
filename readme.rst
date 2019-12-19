This is the proof-of-concept version of NESS. 
See https://github.com/treynr/ness for the production version.


Network Enhanced Similarity Search (NESS)
===========================================

.. image:: https://img.shields.io/travis/treynr/ness.svg?style=flat-square
    :target: https://travis-ci.org/treynr/ness

Advances in genomics have led to improved classifications of 
biological categories and concepts, such as disease, via the analysis 
of high-throughput studies and experimental datasets.
The application of concepts from information theory (e.g., semantic similarity)
is a well-established means to provide a quantitative assessment of the 
similarity of concepts represented in structured vocabularies in biology.
However, the gene and variant annotations which serve as critical components 
of these evaluations are often sparse, potentially biased due to uneven 
representation in the literature, and constrained to the single 
species in which they were derived. 
These deficiencies limit the utility of information content approaches.

NESS is a dual data integration and analysis approach.
It is designed to harmonize large-scale, functional genomics data sources into a 
single cross-species heterogeneous network.
By employing a random walk with restart (RWR) over a harmonized graph structure,
NESS provides data-driven similarity measures of biological concepts across multiple
domains and species.
This flexible approach enables cross-species analysis applications that 
leverage model organism data in the context of sparse datasets. 
Some applications include gene/variant-disease prioritization, disease classification, 
and semantic similarity analysis.


Getting started
---------------

NESS is organized into two components: :code:`nessb`, the heterogeneous network builder
and :code:`nessw` the RWR algorithm.

:code:`nessb` is designed to harmonize disparate data sources (e.g., ontologies, gene
networks, differential expression studies) from multiple species into a single network
structure.
The network builder is relatively quick;
testing on a 56-core machine (Intel Xeon CPU E5-2695 v3 @ 2.30GHz) with 504GB of RAM and
building a network with ~90K nodes and 4.6M edges:

.. code:: bash

    $ time nessb -a go-annotations.tsv -e gene-networks.tsv -g gene-sets.tsv -o go-relations.tsv sample-net.al

    real    0m33.178s
    user    0m32.177s
    sys     0m0.893s

See the `build documentation`__ on how to compile and use :code:`nessb`.

.. __: build/readme.rst

:code:`nessw` applies an RWR algorithm over the harmonized data structure.
Any biological concept(s) in the network can be used as seed nodes.
The RWR implementation is also fairly quick;
testing on a 56-core machine (Intel Xeon CPU E5-2695 v3 @ 2.30GHz) with 504GB of RAM, and
using a network with ~90K nodes and 4.6M edges to calculate the proximity vector of a
single seed node with a restart probability of 0.15:

.. code:: bash

    $ time nessw -a -r 0.15 sample-net.al entity-map-sample-net.al single-seed.txt scores.tsv

    real    0m6.681s
    user    0m6.358s
    sys     0m0.301s

Calculating the complete proximity matrix takes ten or fifteen minutes if distributed
across a cluster.
See the `walk documentation`__ on how to compile and use :code:`nessw`.

.. __: walk/readme.rst


Funding
-------

Part of the GeneWeaver__ data repository and analysis platform.
For a detailed description, see this article__.

.. __: https://geneweaver.org
.. __: https://www.ncbi.nlm.nih.gov/pubmed/26656951

This work has been supported by joint funding from the NIAAA and NIDA, NIH [R01 AA18776];
and `The Jackson Laboratory`__ (JAX) Center for Precision Genetics of the NIH 
[U54 OD020351].

.. __: https://jax.org

