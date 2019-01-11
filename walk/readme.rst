
NESS Walker
===========

:code:`nessw` performs a random walk with restart (RWR) over heterogeneous networks to
calculate affinity between biological entities.

Usage
-----

.. code:: bash

    nessw [options] <graph> <entity-index-map> <seeds> <output>

All arguments to :code:`nessw` are files:

- :code:`<graph>`: the heterogeneous network output provided by :code:`nessb`.
  It should be in the form of an adjacency list but :code:`nessw` can also accept an
  adjacency matrix.

- :code:`<entity-index-map>`: a file containing bioentity-identifier mappings.
  This file is also created and output by :code:`nessb`.

- :code:`<seeds>`: a file containing a list of seed nodes, one per line.

- :code:`<output>`: the output file.


Options
-------

- :code:`-f, --filter=STRING`: filter node types from the output using the given type.
  Using the strings :code:`GENE`, :code:`GS`, :code:`HOM`, will remove gene, gene set,
  and homology nodes respectively.
  Ontology nodes can be filtered out using the ontology term prefix e.g., :code:`GO`,
  :code:`DO`, :code:`MP`, etc.

- :code:`-m, --matrix`: use an adjacency matrix graph representation instead of an
  adjacency list.
  This is **not** recommended for large graphs.
  
- :code:`-v, --verbose`: Clutter your screen with output

- :code:`-h, --help`: Display the help message

Requirements
------------

- gcc >= 4.3

Older versions of gcc may work if they support the C++11 standard.

Installation
------------

From the :code:`walk/` directory:

Compile the walker:

.. code:: bash

    $ make

Run tests:

.. code:: bash

    $ make test

Install to the user specific bin directory (:code:`$HOME/.local/bin`):

.. code:: bash

    $ make install

