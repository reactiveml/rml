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

SPLIT=../tools/split -ext ".rml"
RUN_TEST=../tools/run_test -ext ".rml" -verbose $(VERBOSE) #-good $(COMPILER)

FILES=*.rml
SPLIT_FILES=*.split

DIRS=.

all: realclean split test

split:
	$(SPLIT) $(SPLIT_FILES)

test:
	$(RUN_TEST) $(FILES)

tools:
	(cd ../tools; make)

clean:
	rm -f *annot *.rzi *.ml

realclean: clean
	rm -f *~ *-testgen.rml;

cleanall: realclean
