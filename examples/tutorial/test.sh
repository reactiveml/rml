#!/bin/sh

RMLC=../../compiler/rmlc
RUNTIME=Lk_tutorial

SUCCESS=0
TOTAL=0

for file in *.rml; do
    TOTAL=$((TOTAL + 1))
    f=`basename $file .rml`
    touch $file
    make RMLC="$RMLC -runtime $RUNTIME" FILE=$f > /dev/null 2> /dev/null
    if [ -f ./$f ]; then
	./$f > $f.output 2>&1
	if [ -f $f.result ]; then
	    diff -q $f.output $f.result
	    if [ $? -eq 0 ]; then
		SUCCESS=$((SUCCESS + 1))
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

echo Success: $SUCCESS / $TOTAL
