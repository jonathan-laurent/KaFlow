#!/bin/sh

# First argument: simulation time in second
# Second argument: KaFlow arguments

LEN=$1
ARG=$2

KAFLOW=../../../KaFlow

echo "Building example in directory $1"

rm -rf output ; mkdir output ; cd output

KaSim -i ../*.ka -l $LEN -trace t.json

$KAFLOW t.json -o "story." $ARG

for d in *.dot
do
    dot -Tpdf $d > $d.pdf
done

cd ..
