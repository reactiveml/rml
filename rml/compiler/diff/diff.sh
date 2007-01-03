#! /bin/sh

f () {
    path=$1
    file=$2

    diff -Nau $file.org $path$file > $file.diff

}

f ../reac/ annot.ml
f ../global/ asttypes.mli
f ../main/ errors.ml
f ../global/ global.ml
f ../parsing/ lexer.mll
f ../parsing/ linenum.mll
f ../parsing/ location.ml
f ../global/ modules.ml
f ../parsing/ parse_ast.ml
f ../parsing/ parse.ml
f ../parsing/ parse_printer.ml
f ../parsing/ parser.mly
f ../reac/ reac_ast.ml
f ../parsing/ syntaxerr.ml
f ../global/ warnings.ml

echo "Diff files have been generated."
exit 0