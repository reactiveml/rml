#***************************************************************************#
#*                                Lucy-n                                   *#
#*                                                                         *#
#*                                                                         *#
#* Authors: Louis Mandel (louis.mandel@lri.fr)                             *#
#*          Florence Plateau (florence.plateau@lri.fr)                     *#
#*                                                                         *#
#*                                                                         *#
#* Creation date: September 2011                                           *#
#*                                                                         *#
#***************************************************************************#

# $Id$

OCAMLBUILD= ocamlbuild -I lib -no-links -classic-display \
		-libs str,unix \
		-tags debug,annot

TARGET=native

all: split run_test

native: TARGET := native
native: all
opt: native

byte: TARGET := byte
byte: all


split: split.target
	cp _build/split.$(TARGET) split
split.target:
	$(OCAMLBUILD) split.$(TARGET)


run_test: run_test.target
	cp _build/run_test.$(TARGET) run_test
run_test.target:
	$(OCAMLBUILD) run_test.$(TARGET)


clean: 
	ocamlbuild -clean

realclean: clean
	rm -f *~ split run_test

cleanall: realclean
