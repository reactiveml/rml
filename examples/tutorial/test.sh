#!/bin/sh

# RUNTIME=Lk_tutorial
RUNTIME=Lk

for file in *.rml; do
    f=`basename $file .rml`
    make RMLC="rmlc -runtime $RUNTIME" FILE=$f > /dev/null 2> /dev/null
    if [ -f ./$f ]; then
	./$f > $f.output 2>&1
	if [ -f $f.result ]; then
	    diff $f.output $f.result;
	    if [ $? ]; then
		echo Test of $f: OK
	    else
		echo Test of $f: NOK
	    fi
	else
	    echo $f.result does not exist
	fi
    else
	echo Compilation error: $f.rml
    fi
done

for file in *.rml; do
    f=`basename $file .rml`
    make FILE=$f clean > /dev/null 2&> /dev/null
done
