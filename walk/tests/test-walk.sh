#!/usr/bin/env bash

graph="./data/sample-graph.al"
ent_map="./data/sample-graph.em"
seeds="./data/sample-graph.seed"
results="./data/sample-graph-result.tsv"
output="test.tsv"

## Test the random walk using a small directed graph. These results are similar
## to those found in Yu et al.
## Assumes nessw can be found on the $PATH
.././nessw -r 0.15 "$graph" "$ent_map" "$seeds" "$output" #&& diff "$results" "$output"

declare -a result1
declare -a result2

## Read the output file and the sample results, compare line by line and compare RWR
## scores up to three decimal places. Due to the probabalistic nature of the algorithm,
## scores could differ by ~0.00001 or whatever the threshold is set to.
while read -r line
do
    result1+=("$line")
done < "$results"

while read -r line
do
    result2+=("$line")
done < "$output"

## Files have different number of lines: fail
if [[ "${#result1[@]}" != "${#result2[@]}" ]]; then

    echo "Results have different numbers of lines"
    exit 1
fi

## Compare each line
for (( i=0; i < ${#result1[@]}; i++ ))
do
    ## Split the lines by their tab characters to get the from node, to node, and score
    IFS=$'\t' read -ra line1 <<< "${result1[$i]}"
    IFS=$'\t' read -ra line2 <<< "${result2[$i]}"

    invalid=0

    ## Compare each individual column, rounding the scores to three decimal places
    if [[ ${line1[0]} != ${line2[0]} ]]; then
        invalid=1
    elif [[ ${line1[1]} != ${line2[1]} ]]; then
        invalid=1
    elif [[ `printf %.3f ${line1[2]}` != `printf %.3f ${line2[2]}` ]]; then
        invalid=1
    fi

    if [[ $invalid == 1 ]]; then

        echo "TESTS FAILED"
        echo "Invalid differences:"
        echo "${line1[@]}"
        echo "${line2[@]}"

        exit 1

    else
        echo "ALL TESTS PASSED"

        exit 0
    fi
done
