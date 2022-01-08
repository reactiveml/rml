# Makefile for ReactiveML

build:
	echo "(* This file is generated *)" > compiler/global/version.ml
	echo "let version = \"$(VERSION)\"" >> compiler/global/version.ml
	echo "let stdlib = \"$(LIBDIR)\"" >> compiler/global/version.ml
	dune build stdlib
	RML_RECOMPILE_RZI=0 dune build compiler tools interpreter toplevel

test: build
	dune runtest

install: build
	dune install

uninstall:
	dune uninstall

clean:
	rm -f compiler/global/version.ml
	dune clean
