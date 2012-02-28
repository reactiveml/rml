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

COMPILER="../../compiler/rpmlc.byte -I ../../lib/ -c"

SPLIT=tools/split -ext ".rml"
RUN_TEST=tools/run_test -ext ".rml" -good $(COMPILER) -verbose $(VERBOSE)

FILES= splits/*.rml
SPLIT_FILES= splits/*.split

DIRS= splits

all:

split:
	$(SPLIT) $(SPLIT_FILES)

test:
	$(RUN_TEST) $(FILES)

tools: FORCE
	(cd tools; make)

clean:
	(for dir in $(DIRS); \
	 do rm -f $$dir/*annot $$dir/*.rzi $$dir/*.ml;  \
	 done)
	(cd tools; make clean)

realclean: clean
	rm -f *~
	(for dir in $(DIRS); \
	 do rm -f $$dir/*~ $$dir/*-testgen.rml; \
	 done)
	(cd tools; make realclean)

cleanall: realclean


FORCE:
