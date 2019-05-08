#!/usr/bin/env python
# -*- coding: utf-8 -*-

## file: graph.py
## desc: Functions for build the heterogeneous graph.
## auth: TR

import numpy as np
import networkx as nx

import source

def get_graph_data_sources():
    """

    returns
    """

    t3 = source.get_tier3_sets()
    #t5 = source.get_tier5_sets()
    resources = source.get_public_resources()
    ontologies = source.get_ontologies()

    return {
        'genesets': pd.concat(t3, resources),
        'ontologies': ontologies
    }


def harmonize_datasets(ds):
    """
    Generates node UIDs for all data types in the given dataset.

    arguments
        ds: a dict returned by the function get_graph_data_sources

    returns
        a dict mapping datatypes to a UID
    """

    genesets = ds['genesets']
    ontologies = ds['ontologies']

    ## Begin merging all identifiers to generate UIDs for each of them
    gs_ids = np.array(list(set(genesets.gs_ids.tolist())))
    ont_ids = np.array(list(set(ontologies.left_ont_id.tolist())))
    gene_ids = np.array(list(set(genesets.ode_gene_id.tolist())))
    uids = np.arange(gs_ids.size + ont_ids.size + gene_ids.size)

    gs_uids = dict(zip(np.nditer(gs_ids), np.nditer(uids)))
    ont_uids = dict(zip(np.nditer(ont_ids), np.nditer(uids[gs_ids.size:])))
    gene_uids = dict(zip(np.nditer(gene_ids), np.nditer(uids[(gs_ids.size + ont_ids.size):])))

    return {
        'genesets': gs_uids,
        'ontologies': ont_uids,
        'genes': gene_uids
    }


def build_heterogeneous_graph(ds):
    """

    arguments
        ds:

    returns
        something
    """

    uids = harmonize_datasets(ds)
    genesets = ds['genesets']
    ontologies = ds['ontologies']

    ## Map to their UIDs
    genesets['gs_id'] = genesets.gs_id.replace(uids['genesets'])
    genesets['ode_gene_id'] = genesets.ode_gene_id.replace(uids['genes'])
    ontologies['left_ont_id'] = ontologies.left_ont_id.replace(uids['ontologies'])
    ontologies['right_ont_id'] = ontologies.right_ont_id.replace(uids['ontologies'])

    ## Build the graph
    hetnet = nx.Graph()

    hetnet.add_edges_from([tuple(r) for r in genesets.values])
    hetnet.add_edges_from([tuple(r) for r in ontologies.values])

    return hetnet
