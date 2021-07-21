#! /bin/sh

file=$1
diff -Nau $file ../$file > $file.diff
exit 0
