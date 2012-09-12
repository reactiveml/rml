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

VERBOSE=0

COMPILER=../../compiler/rmlc -I ../../stdlib/ -i

SPLIT=./tools/split -ext ".rml"
RUN_TEST=./tools/run_test -keep-clean -ext ".rml" -verbose $(VERBOSE) #-good $(COMPILER)

FILES= *.rml
SPLIT_FILES= *.split

all: tools-stamp realclean split-stamp test

tools-stamp:
	$(MAKE) -C tools
	touch $@

split-stamp:
	$(SPLIT) $(SPLIT_FILES)
	touch $@

test: split-stamp
	$(RUN_TEST) $(FILES)

test/%.rml:
	$(COMPILER) $(subst test/,,$@)

clean:
	-rm -f *.*annot *.rzi *.ml

realclean: clean
	-rm -f *-testgen.rml *~ split-stamp

cleanall: realclean
