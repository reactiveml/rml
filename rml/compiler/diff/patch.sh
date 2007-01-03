#! /bin/sh


f () {
    path=$1
    file=$2
    if [ -f $path$file ]; then
	echo "$path$file already exists."
    else
	if ! patch -o $path$file $file.org $file.diff; then
	    echo "unable to create $path$file."
	    exit 1
	fi
    fi
}

f ../global/ asttypes.mli
f ../global/ global.ml
f ../global/ modules.ml
f ../global/ warnings.ml
f ../main/ errors.ml
f ../parsing/ lexer.mll
f ../parsing/ linenum.mll
f ../parsing/ location.ml
f ../parsing/ parse_ast.ml
f ../parsing/ parse.ml
f ../parsing/ parse_printer.ml
f ../parsing/ parser.mly
f ../parsing/ syntaxerr.ml
f ../reac/ annot.ml
f ../reac/ reac_ast.ml

exit 0