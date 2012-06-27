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

#COMPILER="../../compiler/rpmlc.byte -I ../../lib/ -c"

SPLIT=./tools/split -ext ".rml"
RUN_TEST=./tools/run_test -keep-clean -ext ".rml" -verbose $(VERBOSE) #-good $(COMPILER)

FILES= *.rml
SPLIT_FILES= *.split

all: realclean split-stamp test

split-stamp:
	$(SPLIT) $(SPLIT_FILES)
	touch split-stamp

test: split-stamp
	$(RUN_TEST) $(FILES)

clean:
	-rm -f *.*annot *.rzi *.ml

realclean: clean
	-rm -f *-testgen.rml *~ split-stamp

cleanall: realclean
