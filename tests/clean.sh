#!/bin/sh

for dir in *
do
    if [ -d $dir ]
    then
       rm -rf $dir/output;
    fi
done
