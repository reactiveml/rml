#!/bin/sh

echo \# D area time 
area=10
until [ $area -eq 1000 ]; do
    let $[ area += 10 ]

    ../simul.opt -N 500 -n 2000 -t 1000 -area $area -D 10 -nox  

    ../simul.opt -N 500 -n 2000 -t 1000 -area $area -D 20 -nox  

    ../simul.opt -N 500 -n 2000 -t 1000 -area $area -D 30 -nox 

done

