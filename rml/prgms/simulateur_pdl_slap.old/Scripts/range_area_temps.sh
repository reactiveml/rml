#!/bin/sh

echo range area time 
area=10
until [ $area -eq 1000 ]; do
    let $[ area += 10 ]

    echo 10 $area 
    time ../simul.opt -n 500 -t 1000 -area $area -range 10 -nox  

    echo 50 $area 
    time ../simul.opt -n 500 -t 1000 -area $area -range 50 -nox  

    echo 100 $area 
    time ../simul.opt -n 500 -t 1000 -area $area -range 100 -nox 
	     						    	 
    echo 200 $area 
    time ../simul.opt -n 500 -t 1000 -area $area -range 200 -nox 

done

