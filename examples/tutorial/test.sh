#!/bin/sh

for file in *.rml; do
    f=`basename $file .rml`
    make $f > /dev/null 2&> /dev/null
    if [ $? ]; then
	./$f 2&>1 > $f.output
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
	echo $f Compilation error
    fi
done
