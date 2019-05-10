#!/usr/bin/env python
# -*- coding: utf-8 -*-

## file: db.py
## desc: Functions for retrieving data from the GeneWeaver database. Mostly ripped
##       from gwlib (https://github.com/treynr/gwlib)
## auth: TR

import pandas as pd

## In gwlib this is a connection pool object, here it will be an instance of the
## ConnectionPoolShim. This should be instantiated by the NESS celery tool app prior
## to calling any of these DB functions.
CONNPOOL = None

## Shim class so I don't have to change the function signatures
class ConnectionPoolShim(object):

    def __init__(self, conn):
        self.conn = conn

    def __enter__(self):
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass


def tuplify(thing):
    """
    Converts a list, list like object or scalar value into a tuple.

    arguments
        thing: some object being converted into a tuple

    returns
        a tuple
    """

    if hasattr(thing, '__iter__'):
        return tuple(thing)

    return (thing,)


def biject(df, key=None, val=None):
    """
    Creates a simple mapping (bijection) of values from one column to another.
    If key or val are not supplied, generates a bijection between values from
    the first and second columns of the given dataframe.
    Duplicates are overwritten.

    arguments
        df:  dataframe returned by one of the DB queries
        key: optional, the column that should be used as the dict key
        val: optional, the column that should be used as the value

    returns
        a bijection of values in one column to another
    """

    if key and val:
        pass

    ## Only key was provided, then assume the value is just the first column after
    ## the key column is removed
    elif key:
        val = df.iloc[:, ~df.columns.isin([key])].columns[0]

    ## Only val was provided, then assume the key is the first column after the
    ## val column is removed
    elif val:
        key = df.iloc[:, ~df.columns.isin([val])].columns[0]

    ## Assume the key is the first column and the value is the second
    else:
        key, val = df.columns[:2]

    return df.set_index(key).to_dict(orient='dict')[val]


def get_geneset_ids(tiers=[1, 2, 3, 4, 5], at_id=None, size=0, sp_id=0):
    """
    Returns an array of normal (i.e. their status is not deleted or deprecated)
    gene set IDs.
    IDs can be filtered based on tiers, gene set size, species, and public resource
    attribution.

    arguments
        at_id: public resource attribution ID
        tiers: a list of curation tiers
        size:  indicates the maximum size a set should be during retrieval
        sp_id: species identifier

    returns
        an array of gene set IDs (gs_id)
    """

    tiers = tuplify(tiers)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT  gs_id
            FROM    production.geneset
            WHERE   gs_status NOT LIKE 'de%%' AND
                    cur_id IN %(tiers)s AND
                    CASE
                        WHEN %(at_id)s IS NOT NULL THEN gs_attribution = %(at_id)s
                        ELSE TRUE
                    END AND
                    CASE
                        WHEN %(size)s > 0 THEN gs_count < %(size)s
                        ELSE TRUE
                    END AND
                    CASE
                        WHEN %(sp_id)s > 0 THEN sp_id = %(sp_id)s
                        ELSE TRUE
                    END;
            ''',
            conn,
            params={'tiers': tiers, 'at_id': at_id, 'size': size, 'sp_id': sp_id}
        ).gs_id.to_numpy()


def get_private_geneset_ids(user=0, size=0, sp_id=0):
    """
    Returns an array of normal (i.e. their status is not deleted or deprecated)
    Tier V (private) gene set IDs.
    Only retrieves sets the user has permission to access.

    arguments
        user:  a user ID, the default user 0 has no access rights to private data
        size:  indicates the maximum size a set should be during retrieval
        sp_id: species identifier to use as a filter

    returns
        an array of gene set IDs (gs_id)
    """

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT  gs_id
            FROM    production.geneset
            WHERE   gs_status NOT LIKE 'de%%' AND
                    --
                    -- geneset_is_readable is a stored procedure that checks user
                    -- permissions and returns true if the user can access the set
                    --
                    geneset_is_readable2(%(user)s, gs_id) AND
                    CASE
                        WHEN %(size)s > 0 THEN gs_count < %(size)s
                        ELSE TRUE
                    END AND
                    CASE
                        WHEN %(sp_id)s > 0 THEN sp_id = %(sp_id)s
                        ELSE TRUE
                    END;
            ''',
            conn,
            params={'user': user, 'size': size, 'sp_id': sp_id}
        ).gs_id.to_numpy()


def get_geneset_values(gs_ids):
    """
    Returns a dataframe containing gene set values (genes and scores) for the given
    list of gene set IDs.

    arguments
        gs_ids: a list of gene set identifiers

    returns
        a dataframe containing gene set IDs (gs_id), GW gene IDs (ode_gene_id), and
        scores (gsv_value)
    """

    gs_ids = tuplify(gs_ids)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT gs_id, ode_gene_id, gsv_value
            FROM   extsrc.geneset_value
            WHERE  gs_id IN %s;
            ''',
            conn,
            params=(gs_ids,)
        )


def get_gene_homologs(genes, source='Homologene'):
    """
    Returns a dataframe contaiing internal GW homology IDs for the given
    list of gene IDs.

    arguments
        genes:  list of internal GeneWeaver gene identifiers (ode_gene_id)
        source: the homology mapping data source to use, default is Homologene

    returns
        a dataframe containing GW gene IDs (ode_gene_id) and their associated
        homology IDs (hom_id)
    """

    genes = tuplify(genes)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT ode_gene_id, hom_id
            FROM   extsrc.homology
            WHERE  ode_gene_id IN %s AND
                   hom_source_name = %s;
            ''',
            conn,
            params=(genes, source)
        )


def get_attributions():
    """
    Returns all the attributions (at_id and at_abbrev) found in the DB.
    These represent third party data resources integrated into GeneWeaver.

    returns
        a dataframe of attribution abbreviations (at_abbrev) and their
        identifiers (at_id)
    """

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT at_abbrev, at_id
            FROM   odestatic.attribution;
            ''',
            conn
        )


def get_geneset_annotations(gs_ids):
    """
    Returns gene set annotations for the given list of gene set IDs.

    arguments
        gs_ids: list of gene set ids to retrieve annotations for

    returns
        a dataframe containing geneset annotations which include the internal GW
        ontology id (ont_id) and the ontology term reference (ont_ref_id)
    """

    gs_ids = tuplify(gs_ids)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT      go.gs_id, go.ont_id, o.ont_ref_id
            FROM        extsrc.geneset_ontology AS go
            INNER JOIN  extsrc.ontology AS o
            USING       (ont_id)
            WHERE       gs_id IN %s;
            ''',
            conn,
            params=(gs_ids,)
        )


def get_ontologies():
    """
    Returns the list of ontologies supported by GeneWeaver for use with gene
    set annotations.

    returns
        a dataframe containing ontologies supported by GW
    """

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT ontdb_id, ontdb_name, ontdb_prefix, ontdb_linkout_url, ontdb_date
            FROM   odestatic.ontologydb;
            ''',
            conn
        )


def get_ontology_terms_by_ontdb(ontdb_id):
    """
    Retrieves all ontology terms associated with the given ontology.

    args
        ontdb_id: the ID representing an ontology

    returns
        a dataframe containing ontology term metadata for a given ontology
    """

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT *
            FROM   extsrc.ontology
            WHERE  ontdb_id = %s;
            ''',
            conn,
            params=(ontdb_id,)
        )


def get_ontology_relations(left=None, right=None):
    """
    Retrieves ontology relationships.

    args:
        left: optional set of ontology IDs (ont_id) for the left side relation
        right: optional set of ontology IDs (ont_id) for the right side relation

    returns
        a dataframe containing ontology relations
    """

    if left:
        left = tuplify(left)

    if right:
        right = tuplify(right)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT *
            FROM   extsrc.ontology_relation
            WHERE  
                   CASE
                       WHEN %(left)s IS NOT NULL
                       THEN left_ont_id IN %(left)s
                       ELSE TRUE
                   END
                   OR
                   CASE
                       WHEN %(right)s IS NOT NULL
                       THEN right_ont_id IN %(right)s
                       ELSE TRUE
                   END
            ''',
            conn,
            params={'left': left, 'right': right}
        )


def get_all_ontology_relations(ontdb_id):
    """
    Retrieves all ontology relationships associated with the given ontology DB ID
    (ontdb_id).

    args:
        ontdb_id: ontology DB ID

    returns
        a dataframe containing ontology relations
    """

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            WITH ont_ids AS (
                SELECT ont_id
                FROM   extsrc.ontology
                WHERE  ontdb_id = %(ontdb_id)s
            )
            SELECT *
            FROM   extsrc.ontology_relation
            WHERE  left_ont_id IN (SELECT * FROM ont_ids) OR
                   right_ont_id IN (SELECT * FROM ont_ids);
            ''',
            conn,
            params={'ontdb_id': ontdb_id}
        )


## The following functions are used to get metadata when displaying NESS results.

def get_gene_metadata(genes):
    """
    Retrieve gene metadata for the given set of genes (ode_gene_id).

    arguments
        genes: a list of gene identifiers (ode_gene_id)

    returns
        a dataframe containing gene metadata
    """

    genes = tuplify(genes)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            WITH ensembl_id AS (
                SELECT gdb_id
                FROM   odestatic.genedb
                WHERE  gdb_shortname = 'ensembl'
                LIMIT  1
            )
            SELECT    DISTINCT ON (gi.ode_gene_id)
                      gi.ode_gene_id,
                      g1.ode_ref_id AS symbol,
                      g2.ode_ref_id AS ensembl,
                      s.sp_name AS species
            FROM      extsrc.gene_info gi
            LEFT JOIN extsrc.gene g1
            USING     (ode_gene_id)
            LEFT JOIN extsrc.gene g2
            USING     (ode_gene_id)
            LEFT JOIN odestatic.species s
            ON        gi.sp_id = s.sp_id
            WHERE     gi.ode_gene_id IN %s AND
                      g1.ode_pref AND
                      g2.gdb_id = (SELECT * FROM ensembl_id);
            ''',
            conn,
            params=(genes,)
        )


def get_geneset_metadata(genesets):
    """
    Retrieve gene set metadata for the given set of gene set identifiers (gs_id).

    arguments
        genesets: a list of gene set identifiers (gs_id)

    returns
        a dataframe containing gene set metadata
    """

    genesets = tuplify(genesets)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT    gs.gs_id,
                      gs.cur_id AS tier,
                      COALESCE(at.at_abbrev, 'Curated') AS attribution,
                      s.sp_name AS species,
                      gs.gs_name AS name
            FROM      production.geneset gs
            LEFT JOIN odestatic.species s
            ON        gs.sp_id = s.sp_id
            LEFT JOIN odestatic.attribution at
            ON        gs.gs_attribution = at.at_id
            WHERE     gs.gs_id IN %s;
            ''',
            conn,
            params=(genesets,)
        )


def get_ontology_metadata(onts):
    """
    Retrieve ontology metadata for the given set of ontology identifiers (ont_id).

    arguments
        onts: a list of ontology identifiers (ont_id)

    returns
        a dataframe containing ontology metadata
    """

    onts = tuplify(onts)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            SELECT    ont.ont_id,
                      odb.ontdb_name AS ontology,
                      odb.ontdb_prefix AS ontology_prefix,
                      ont.ont_ref_id AS term_id,
                      ont.ont_name AS name 
            FROM      extsrc.ontology ont
            LEFT JOIN odestatic.ontologydb odb
            USING     (ontdb_id)
            WHERE     ont.ont_id IN %s;
            ''',
            conn,
            params=(onts,)
        )


def get_homology_metadata(homs):
    """
    Retrieve homology metadata for the given set of homology identifiers (hom_id).

    arguments
        homs: a list of homology identifiers (hom_id)

    returns
        a dataframe containing homology metadata
    """

    homs = tuplify(homs)

    with CONNPOOL as conn:
        return pd.read_sql_query(
            '''
            WITH ensembl_id AS (
                SELECT gdb_id
                FROM   odestatic.genedb
                WHERE  gdb_shortname = 'ensembl'
                LIMIT  1
            )
            SELECT    DISTINCT ON (h.ode_gene_id)
                      h.hom_id,
                      g1.ode_ref_id AS symbol,
                      g2.ode_ref_id AS ensembl,
                      sp.sp_name AS species
            FROM      extsrc.homology h
            LEFT JOIN extsrc.gene g1
            ON        h.ode_gene_id = g1.ode_gene_id
            LEFT JOIN extsrc.gene g2
            ON        h.ode_gene_id = g2.ode_gene_id
            LEFT JOIN odestatic.species sp
            ON        h.sp_id = sp.sp_id
            WHERE     h.hom_id IN %s AND
                      g1.ode_pref AND
                      g2.gdb_id = (SELECT * FROM ensembl_id);
            ''',
            conn,
            params=(homs,)
        )
