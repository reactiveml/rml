#!/bin/sh


n=2550

echo "" > stat.data

until [ $n -eq 500000 ]; do
    let $[ n += 100 ]
    ../simul.opt -N 500 -n $n -t 1000 -D 20 -nox >> stat.data
    ../simul.opt -N 500 -n $n -t 1000 -D 20 -area 1000 -nox >> stat.data
done