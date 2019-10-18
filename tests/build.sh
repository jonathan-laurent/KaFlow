#!/bin/sh

# First argument: simulation time in second
# Second argument: KaFlow arguments

KASIM_ARGS=$1
KAFLOW_ARGS=$2

KAFLOW=../../../KaFlow

echo "KaSim arguments: $KASIM_ARGS"
echo "KaFlow arguments: $KAFLOW_ARGS"

rm -rf output ; mkdir output ; cd output

KaSim $KASIM_ARGS -i ../*.ka -trace t.json

$KAFLOW t.json -o "story." $KAFLOW_ARGS

for d in *.dot
do
    dot -Tpdf $d > $d.pdf
done

cd ..
