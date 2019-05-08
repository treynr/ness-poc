#!/usr/bin/env python
# -*- coding: utf-8 -*-

## file: test_graph.py
## desc: Tests for the graph.py module.
## auth: TR

from __future__ import print_function
import copy
import networkx as nx
import pandas as pd
import pytest

from src import graph


@pytest.fixture(scope='module')
def _genesets():
    return pd.DataFrame(
        data=[
            [1, 100],
            [1, 101],
            [1, 102],
            [2, 100],
            [2, 103],
            [2, 104],
            [3, 102],
            [3, 103],
            [3, 105]
        ],
        columns=['gs_id', 'ode_gene_id']
    )


@pytest.fixture(scope='module')
def _ontologies():
    return pd.DataFrame(
        data=[[4, 1], [3, 2], [6, 3], [7, 1], [2, 1]],
        columns=['left_ont_id', 'right_ont_id']
    )


@pytest.fixture(scope='module')
def _homology():
    return pd.DataFrame(
        data=[[100, 200], [101, 201], [102, 202], [103, 200]],
        columns=['ode_gene_id', 'hom_id']
    )


@pytest.fixture(scope='module')
def _annotations():
    return pd.DataFrame(
        data=[[1, 1], [1, 4], [2, 3], [2, 6]],
        columns=['gs_id', 'ont_id']
    )


@pytest.fixture(scope='module')
def _datasets(_annotations, _genesets, _homology, _ontologies):
    return {
        'annotations': _annotations,
        'genesets': _genesets,
        'homology': _homology,
        'ontologies': _ontologies
    }


@pytest.fixture(scope='module')
def _graph():
    test_graph = nx.Graph()
    test_graph.add_edges_from([
        (0, 3), (0, 9), (0, 10), (0, 11),
        (0, 6), (1, 9), (1, 5), (1, 7), (2, 9), (2, 11),
        (3, 8), (3, 4), (3, 6), (4, 5), (5, 7)
    ])

    return test_graph


def test_update_homology_ids(_genesets, _homology):

    new_sets = graph.update_homology_ids(_genesets.copy(deep=True), _homology)

    assert len(new_sets.index) == 7
    assert new_sets[new_sets.gs_id == 1].ode_gene_id.isin([200, 201, 202]).all()
    assert new_sets[new_sets.gs_id == 2].ode_gene_id.isin([200]).all()
    assert new_sets[new_sets.gs_id == 3].ode_gene_id.isin([200, 202]).all()

def test_harmonize_datasets(_datasets):

    uids = graph.harmonize_datasets(copy.deepcopy(_datasets))

    assert len(uids['genesets'].keys()) == 3
    assert len(uids['ontologies'].keys()) == 6
    assert len(uids['genes'].keys()) == 3

    assert uids['genesets'].values() == [0, 1, 2]
    assert uids['ontologies'].values() == [3, 4, 5, 6, 7, 8]
    assert uids['genes'].values() == [9, 10, 11]

def test_build_heterogeneous_graph(_datasets, _graph):

    uids = graph.harmonize_datasets(_datasets)
    hetnet = graph.build_heterogeneous_graph(_datasets, uids)

    assert nx.is_isomorphic(hetnet, _graph)

def test_generate_adjacency_list(_graph):

    alist = graph.generate_adjacency_list(_graph)

    ## Just check a subset
    assert sorted(alist[0]) == [3, 6, 9, 10, 11]
    assert sorted(alist[1]) == [5, 7, 9]
    assert sorted(alist[2]) == [9, 11]
    assert sorted(alist[3]) == [0, 4, 6, 8]

