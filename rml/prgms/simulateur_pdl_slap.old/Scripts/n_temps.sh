#!/bin/sh


n=3500

#echo "" > stat.data

until [ $n -eq 3700 ]; do
    let $[ n += 100 ]
#    ../simul.opt -N 1000 -n $n -t 1000 -msg_proba 10 -D 10 -nox >> stat.data
    ../simul.opt -N 1000 -n $n -t 1000 -msg_proba 10 -D 20 -nox >> stat.data
#    ../simul.opt -N 1000 -n $n -t 1000 -msg_proba 10 -D 30 -nox >> stat.data
done

date
