#!/bin/sh

verbose=
if [ "$1" = "-v" ]; then
  verbose='y'
  shift
fi

runtimes="Lco_rewrite Lco_ctrl_tree Lco_ctrl_tree_n Lco_ctrl_tree_class Lk"
if [ -n "$1" ]; then
  runtimes=$1
fi

for run in $runtimes; do
  echo "============================================"
  echo "Testing runtime $run…"
  echo "============================================"
  ../../compiler/rmlc -runtime $run async1.rml
  ocamlc -thread -rectypes -I ../../interpreter unix.cma threads.cma rmllib.cma async1.ml -o /dev/null 2>&1
  if [ $? -ne 0 ]; then
    [ -n "$verbose" ] && cat -n async1.ml
  else
    echo "Ok!"
  fi
  echo
done
