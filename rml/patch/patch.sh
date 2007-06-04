#! /bin/sh

file=$1

if [ -f ../$file ]; then
    echo "../$file already exists."
else
    if ! patch -o ../$file $file $file.diff; then
	echo "unable to create ../$file."
	exit 1
    fi
fi

exit 0
