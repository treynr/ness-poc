#!/usr/bin/env python
# -*- coding: utf-8 -*-

## file: graph.py
## desc: Functions for build the heterogeneous graph.
## auth: TR

from __future__ import print_function
import numpy as np
import networkx as nx
import pandas as pd
import sys

#from . import db
#from . import source
import db
import source


def get_graph_data_sources(user=0):
    """
    Retrieve data sources from GeneWeaver used to build the graph.

    arguments
        user: a user ID for retrieving private gene sets

    returns
        a dict containing GS annotations, gene sets, homology IDs, and ontology relations
    """

    t3 = source.get_tier3_sets()
    #t5 = source.get_tier5_sets()
    resources = source.get_public_resources()
    ontologies = source.get_ontologies()
    genesets = pd.concat([t3, resources])
    annotations = source.get_geneset_annotations(set(genesets.ode_gene_id.tolist()))
    homology = db.get_gene_homologs(
        set(genesets.ode_gene_id.tolist()),
        source='Homologene'
    )

    return {
        'annotations': annotations,
        'genesets': genesets,
        'homology': homology,
        'ontologies': ontologies
    }


def update_homology_ids(genesets, homology):
    """
    Convert gene IDs (ode_gene_id) to homology cluster IDs (hom_id).

    arguments
        genesets: a dataframe containing genesets

    returns
        a dataframe containing genesets
    """

    ## Convert the ode_gene_id, hom_id dataframe to a mapping (dict) struct
    hom_map = db.biject(homology)

    ## Update ode_gene_id -> hom_id
    genesets['ode_gene_id'] = genesets.ode_gene_id.map(hom_map)

    ## Drop rows w/ missing homology identifiers
    genesets = genesets.dropna()

    return genesets


def harmonize_datasets(ds, add_homology=False):
    """
    Generates node UIDs for all data types in the given dataset.

    arguments
        ds:           a dict returned by the function get_graph_data_sources
        add_homology: if add_homology is true, then the function will also create UIDs
                      for each homology cluster so they can be linked to edges in the
                      resulting graph

    returns
        a dict mapping datatypes to a UID
    """

    genesets = ds['genesets']
    homology = ds['homology']
    ontologies = ds['ontologies']

    ## Begin merging all identifiers to generate UIDs for each of them
    gs_ids = np.array(list(set(genesets.gs_id.tolist())), dtype=np.int64)
    ont_ids = np.array(list(set(
        ontologies.left_ont_id.tolist() + ontologies.right_ont_id.tolist()
    )), dtype=np.int64)
    gene_ids = np.array(list(set(genesets.ode_gene_id.tolist())), dtype=np.int64)

    uids = np.arange(gs_ids.size + ont_ids.size + gene_ids.size)

    gs_uids = dict(zip(gs_ids, uids))
    ont_uids = dict(zip(ont_ids, uids[gs_ids.size:]))
    gene_uids = dict(zip(gene_ids, uids[(gs_ids.size + ont_ids.size):]))
    hom_uids = {}

    if add_homology:
        hom_ids = np.array(list(set(homology.hom_id.tolist())), dtype=np.int64)
        hom_uids = np.arange(uids.size, uids.size + hom_ids.size)
        hom_uids = dict(zip(hom_ids, hom_uids))

    return {
        'genesets': gs_uids,
        'ontologies': ont_uids,
        'genes': gene_uids,
        'homology': hom_uids
    }


def build_heterogeneous_graph(ds, uids, add_homology=False):
    """
    Build the heterogeneous graph structure.

    arguments
        ds:           datasets from GeneWeaver to use
        uid_map:      mapping to convert GWIDs into graph UIDs
        add_homology: if add_homology is true, the function will add homology clusters
                      to the graph as a separate node type and link genes to each
                      cluster

    returns
        the graph
    """

    genesets = ds['genesets']
    ontologies = ds['ontologies']
    annotations = ds['annotations']

    ## Map to their UIDs
    annotations['gs_id'] = annotations.gs_id.map(uids['genesets'])
    annotations['ont_id'] = annotations.ont_id.map(uids['ontologies'])

    genesets['gs_id'] = genesets.gs_id.map(uids['genesets'])
    genesets['ode_gene_id'] = genesets.ode_gene_id.map(uids['genes'])

    ontologies['left_ont_id'] = ontologies.left_ont_id.map(uids['ontologies'])
    ontologies['right_ont_id'] = ontologies.right_ont_id.map(uids['ontologies'])

    ## Remove anything that wasn't mapped to a UID
    annotations = annotations.dropna()
    genesets = genesets.dropna()
    ontologies = ontologies.dropna()

    ## Force to integers
    annotations = annotations.astype({'gs_id': np.int64, 'ont_id': np.int64})
    genesets = genesets.astype({'gs_id': np.int64, 'ode_gene_id': np.int64})
    ontologies = ontologies.astype({
        'left_ont_id': np.int64, 'right_ont_id': np.int64
    })

    ## Build the graph
    hetnet = nx.Graph()

    hetnet.add_edges_from([tuple(r) for r in genesets.values])
    hetnet.add_edges_from([tuple(r) for r in ontologies.values])
    hetnet.add_edges_from([tuple(r) for r in annotations.values])

    if add_homology:
        homology = ds['homology']
        homology['hom_id'] = homology.hom_id.map(uids['homology'])
        homology['ode_gene_id'] = homology.ode_gene_id.map(uids['genes'])

        homology = homology.dropna()
        homology = homology.astype({'hom_id': np.int64, 'ode_gene_id': np.int64})

        hetnet.add_edges_from([tuple(r) for r in homology.values])

    return hetnet


def generate_adjacency_list(graph):
    """
    Formats the graph as an adjacency list.

    arguments
        graph: a NetworkX graph object

    returns
        a dict of node ids and the nodes they are connected to
    """

    adjacency = {}

    for node, edges in graph.adjacency():

        if node not in adjacency:
            adjacency[node] = set([])

        adjacency[node].update(edges.keys())

    for node in adjacency.keys():
        adjacency[node] = list(adjacency[node])

    return adjacency


def write_adjacency_list(fp, alist):
    """
    Write the adjacency list to a file in a format that can be accepted by the NESS
    RWR program.

    arguments
        fp:    the filepath
        alist: the graph represented as an adjacency list
    """

    with open(fp, 'w') as fl:
        ## The file format requires the size of the graph as the first line. It can be
        ## any number since it isn't actually used.
        print(100, file=fl)

        for k in sorted(alist.keys()):
            edges = ','.join([str(e) for e in alist[k]])

            print(edges, file=fl)


def write_harmonization_map(fp, uids):
    """
    Write the GW entity -> UID mapping to a file.

    arguments
        fp:  the filepath
        uid: the UID mapping
    """

    with open(fp, 'w') as fl:
        print(0, file=fl)

        for uidtype in uids.keys():
            for k, v in uids[uidtype].items():

                ## Prefix IDs from GW since they may overlap, the tool's web interface
                ## will submit prefixed IDs like this anyway
                if uidtype == 'genesets':
                    prefix = 'GSID:'
                elif uidtype == 'ontologies':
                    prefix = 'ONTID:'
                elif uidtype == 'homology':
                    prefix = 'HOM:'
                else:
                    prefix = 'ODE:'

                print('\t'.join([prefix + str(k), str(v)]), file=fl)

