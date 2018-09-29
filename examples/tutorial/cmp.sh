#!/bin/sh

FILE1=$1
FILE2=$2

diff $FILE1 $FILE2 > /dev/null
if [ $? ]; then
    echo $FILE1 OK
else
    echo $FILE2 NOK
fi
