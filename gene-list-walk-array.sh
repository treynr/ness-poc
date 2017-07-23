#!/bin/bash
#PBS -N gene-walk
#PBS -l nodes=1:ppn=3
#PBS -l mem=400gb
#PBS -l walltime=72:00:00
#PBS -e out-gene-walk.txt
#PBS -o err-gene-walk.txt

## file: batch.sh
## desc: Submitting the graph walk program to a cluster.
##
##       job submission: qsub -J 1-10 gene-list-walk-array.sh
##
## vers: 0.1.0
## auth: TR
#

base_dir="/projects/chesler-lab"
## Directory with sim executable
this_dir="$base_dir/walker"
## Data directory
data_dir="$base_dir/walker-data"
## Edge list file
edges="--edges $data_dir/el-exp-no-kegg-hsa.tsv"  
## GO annotations
annotations="--annotations $data_dir/goa-hsa-gw.tsv"
## GO term relationships
terms="--ontology $data_dir/go-relations.tsv"
## GW gene sets
genesets="--genesets $data_dir/gs-exp-hsa.tsv"
filters="--save-genes"
input="--input-file $data_dir/genes-pm-kegg-hsa-filtered-${PBS_ARRAYID}.tsv"
## Stack executable
stack="$HOME/.local/bin/stack"
## Output filepath
output="$data_dir/kegg-pathway-gene-walk-${PBS_ARRAYID}.tsv"

## Only does this if the script is submitted to the batch system
#if [ -z ${PBS_O_WORKDIR+x} ]; then

	## The job must be submitted from the semantic-similarity directory
	## otherwise the batch system may throw an error about not finding the 
	## Haskell compiler
	cd $PBS_O_WORKDIR
#fi

mkdir -p $data_dir

## Let the user specify the type of similarity measurement by using the SIM 
## environment variable
#if [[ "x" == "x$SIM" ]]; then
#
#	SIM="resnik"
#fi
#
### Let the user specify an output filepath by using the OUT environment
### variable
#if [[ "x" != "x$OUT" ]]; then
#
#	output="$OUT"
#fi
#
### Let the user filter out irrelevant ontology namespaces by using the NS 
### environment variable
#if [[ "x" == "x$NS" ]]; then
#
#	NS=""
#
#else
#
#	output="${output}-$NS"
#fi
#
#set -- "$SIM" "$NS"

$stack exec --RTS -- walker --verbose $filters $input $edges $annotations $genesets $terms $output +RTS -N3 -qg
#$stack exec --RTS -- walker --verbose $filters $input $edges $genesets $output +RTS -N3 -qg


