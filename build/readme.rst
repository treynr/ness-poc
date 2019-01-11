
NESS Builder
============

:code:`nessb` builds heterogeneous networks from a variety of disparate functional
genomics data sources.

Usage
-----

.. code:: bash

    nessb [OPTIONS] <output-file>

Lets say you have a series of datasets: some structured (as a DAG) ontology, ontology
annotations, and gene sets (differential expression studies, etc.).
A single graph can be built from these sources:

.. code:: bash

    $ nessb -o ontology.tsv -a annotations.tsv -g genesets.tsv graph.al

This above :code:`nessb` command will generate two files: :code:`graph.al` which contains
the harmonized network in the form of an adjacency list, and :code:`entity-map-graph.al`, 
which maps internal NESS identifiers used for concept representation back to identifiers 
from the user supplied data sources (:code:`ontology.tsv`, :code:`annotations.tsv`, and 
:code:`genesets.tsv`).

Options
-------

- :code:`-a, --annotations=FILE`: Integrate ontology annotations into the network

- :code:`-e, --edges=FILE`: Integrate the contents of the edge list file into the network
  
- :code:`-g, --genesets=FILE`: Integrate gene sets into the network

- :code:`-h, --homologs=FILE`: Integrate homology associations among genes into the network

- :code:`-o, --ontology=FILE`: Integrate ontology structures and relationships into the network

- :code:`-d, --directed`: Build a directed graph (undirected graphs are built by default)

- :code:`--permute=N`: Generate up to N graph permutations for permutation testing

- :code:`-v, --verbose`: Clutter your screen with output

- :code:`--help`: Display the help message

File Formats
------------

Input files use a simple tab delimited format.
All gene (which includes annotated and homologous genes) and gene set identifiers
**must** be integers.
If using something like gene symbols, these symbols must be mapped to some unique integer
identifier prior to their use as :code:`nessb` input.
:code:`nessb` will likely be updated in the future to accomodate string-based identifiers.

Annotations
'''''''''''

The annotation file contains ontology term and gene associations (annotations) in the 
following format:

+------------+---------+---------------+------------+
| term_id    | gene_id | evidence_code | species_id |
+============+=========+===============+============+
| GO:0051960 | 11      | TAS           | 9606       |
+------------+---------+---------------+------------+
| GO:0051960 | 16      | TAS           | 9606       |
+------------+---------+---------------+------------+
| GO:0007399 | 11      | TAS           | 9606       |
+------------+---------+---------------+------------+

Each annotation file should have a minimum of four fields: :code:`term_id`,
:code:`gene_id`, :code:`evidence_code`, and :code:`species_id`.

- :code:`term_id` is a unique identifier string which represents a single ontology term.

- :code:`gene_id` is a unique identifier which a gene, gene product, or variant. 
  It must be an integer.

- :code:`evidence_code` is some identifier string which describes the evidence supporting
  a given term and gene association. 
  If no evidence is provided, this field can be set to any character such as '.' or '_'.

- :code:`species_id` is a unique integer identifier representing the species for a given
  annotation.
  `NCBI taxon IDs`__ are commonly used.

.. __: https://ncbi.nlm.nih.gov/taxonomy


Edges
'''''

The edge file represents gene networks using edge list pairs in the following format:

+------------+------+----+
| species_id | from | to |
+============+======+====+
| 9606       | 11   | 12 |
+------------+------+----+
| 9606       | 16   | 12 |
+------------+------+----+
| 9606       | 12   | 13 |
+------------+------+----+

All fields are required.

- :code:`species_id` is a unique integer identifier representing the species for a given
  annotation.

- The :code:`from` and :code:`to` fields are both unique integer IDs representing a gene
  or bioentity. These fields indicate a directed edge originates at :code:`from` and
  points toward :code:`to`

Gene sets
'''''''''

The gene set file represents a collection of genes pertaining to some biological process
or state in the following format:

+------------+------------+----------+
| geneset_id | species_id | genes    |
+============+============+==========+
| 246376     | 9606       | 10|11|12 |
+------------+------------+----------+
| 75605      | 9606       | 16|15    |
+------------+------------+----------+
| 246626     | 9606       | 13       |
+------------+------------+----------+

All fields are required.

- :code:`geneset_id` is a unique integer identifier representing a given gene set

- :code:`species_id` is a unique integer identifier representing the species for a given
  annotation.

- The :code:`genes` field contains a list of gene identifiers present in the set.
  Like all gene identifiers, these should be integers.
  Individual genes are pipe delimited "|".

Ontology relationsips
'''''''''''''''''''''

The ontology file represents a series of child-parent ontology term relationships.
Ideally, reassembly of these edges would form a directed acyclic graph (DAG).
This file is in a format identical to the edge list format but allows for string based
identifiers:

+------------+------------+
| child      | parent     |
+============+============+
| GO:0051960 | GO:0007399 |
+------------+------------+
| GO:0022008 | GO:0007399 |
+------------+------------+
| GO:0021675 | GO:0007399 |
+------------+------------+
| GO:0007399 | GO:0048731 |
+------------+------------+

All fields are required.

- The :code:`child`  and :code:`parent` fields should each be ontology term identifiers.
  They represent a child &#8594; parent (subconcept &#8594; superconcept) relationship.


Requirements
------------

- GHC 8.2.2
- Stack__

.. __: https://www.haskellstack.org


Installation
------------

See the stack website__ for instructions on installing stack.
After installing stack, make sure it's available on your PATH.

.. __: https://www.haskellstack.org

Compile the builder:

.. code:: bash

    $ make

Run tests:

.. code:: bash

    $ make test

Install to the user specific bin directory (usually :code:`$HOME/.local/bin`):

.. code:: bash

    $ make install

