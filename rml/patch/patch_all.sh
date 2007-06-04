#! /bin/sh

for file in `cat files`; do ./patch.sh $file; done
