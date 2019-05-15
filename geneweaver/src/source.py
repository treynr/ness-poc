#!/usr/bin/env python
# -*- coding: utf-8 -*-

## file: source.py
## desc: Functions for retrieving data from the resources used by NESS.
## auth: TR

from __future__ import print_function
import pandas as pd
import sys

#from . import db
import db

## Hardcoded public resource datasets to retrieve. We exclude some since it either
## doesn't make sense to include them or we retrieve them from other sources.
_ontologies = ['GO', 'MP', 'MA', 'MESH', 'CHEBI', 'DO', 'EFO', 'HPO']
#_resources = ['ABA', 'CTD', 'GWAS', 'DRG', 'MSIGDB', 'OMIM', 'KEGG']
#_annotations = ['GO', 'HP', 'MESH', 'MP']
_resources = [
    'ABA', 'CTD', 'GWAS', 'DRG', 'MSIGDB', 'OMIM', 'KEGG', 'GO', 'HP', 'MESH', 'MP'
]
_annotations = []


def get_tier3_sets():
    """
    Retrieve T-III sets from the GWDB.

    returns
        a dataframe containing a gene set ID (gs_id) and gene ID (ode_gene_id) per row.
    """

    gsids = db.get_geneset_ids(tiers=[3])
    values = db.get_geneset_values(gsids)

    ## Drop the value column since we don't use it
    values = values.drop(['gsv_value'], axis=1)

    return values


def get_tier5_sets(user=0):
    """
    Retrieve T-V sets from the GWDB.

    arguments
        user: the usr_id

    returns
        a dataframe containing a gene set ID (gs_id) and gene ID (ode_gene_id) per row.
    """

    gsids = db.get_private_geneset_ids(user=user)
    values = db.get_geneset_values(gsids)

    ## Drop the value column since we don't use it
    values = values.drop(['values'], axis=1)

    return values


def get_public_resource_sets(resource):
    """
    Retrieve sets associated with an integrated public resource.

    arguments
        resource:

    returns
        a dataframe containing a gene set ID (gs_id) and gene ID (ode_gene_id) per row.
    """

    attributions = db.get_attributions()
    attributions['at_abbrev'] = attributions.at_abbrev.str.lower()

    if resource.lower() not in attributions.at_abbrev:
        return pd.DataFrame()

    at_id = attributions[attributions['at_abbrev'] == resource.lower()].at_id

    gsids = db.get_geneset_ids(at_id=at_id)
    values = db.get_geneset_values(gsids)

    ## Drop the value column since we don't use it
    values = values.drop(['gsv_value'], axis=1)

    return values


def get_ontology(ontology):
    """
    Retrieve ontology terms associated with the given ontology and rebuild the DAG.

    arguments
        resource:

    returns
        a dataframe containing a gene set ID (gs_id) and gene ID (ode_gene_id) per row.
    """

    ontologies = db.get_ontologies()
    ontologies['ontdb_prefix'] = ontologies.ontdb_prefix.str.lower()

    if ontology.lower() not in ontologies.ontdb_prefix.values:
        return pd.DataFrame(data=[], columns=['left_ont_id', 'right_ont_id'])

    ontdb_id = ontologies[ontologies['ontdb_prefix'] == ontology.lower()].ontdb_id.iloc[0]

    relations = db.get_all_ontology_relations(ontdb_id)

    ## Drop the relation type
    relations = relations.drop(['or_type'], axis=1)

    return relations


def get_ontologies(ontologies=_ontologies):
    """
    Retrieve ontology terms associated with all ontologies.

    returns
        a dataframe containing all ontology ID (ont_id) relations
    """

    onts = []

    for ont in ontologies:
        onts.append(get_ontology(ont))

    return pd.concat(onts)


def get_public_resources(resources=_resources):
    """
    Retrieve all public resource sets.

    returns
        a dataframe containing all resource gene sets
    """

    sets = []

    for res in resources:
        sets.append(get_public_resource_sets(res))

    return pd.concat(sets)


def get_geneset_annotations(gsids):
    """
    Retrieve gene set annotations for the given set of gene set IDs.

    arguments
        gsids: a list of gene set IDs (gs_id)

    returns
        a dataframe containing the gene set ID (gs_id) and GW ontology ID (ont_id) per
        row.
    """

    annotations = db.get_geneset_annotations(gsids)

    return annotations[['gs_id', 'ont_id']]


def get_term_annotations(ontologies=_annotations):
    """
    Retrieve genes annotated to ontology terms. These are represented as sets, so we
    retrieve the sets and link each gene in the set to the ontology term.

    arguments
        ontologies: a list of ontology attributions

    returns
        a dataframe containing the GW ontology ID (ont_id) and gene ID (ode_gene_id) per
        row.
    """

    ## Get ontology prefix -> ontdb_id mappings
    ontdb_ids = db.get_ontologies()
    ontdb_ids['ontdb_prefix'] = ontdb_ids.ontdb_prefix.str.lower()
    annotations = []

    for ont in ontologies:
        if ont.lower() not in ontdb_ids.ontdb_prefix.values:
            continue

        ontdb_id = ontdb_ids[ontdb_ids['ontdb_prefix'] == ont.lower()].ontdb_id.iloc[0]

        annotations.append(db.get_term_annotations(ontdb_id))

    if annotations:
        return pd.concat(annotations)

    return pd.DataFrame([], columns=['ont_id', 'ode_gene_id'])


def get_homologs(genes):
    """
    Retrieve homology mappings for the given set of genes (ode_gene_id).

    arguments
        genes: a list of genes (ode_gene_id)

    returns
        a dataframe containing ode_gene_id and hom_id associations
    """

    ## Remove duplicates
    genes = list(set(genes))

    return db.get_gene_homologs(genes, source='Homologene')

