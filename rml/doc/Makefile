include ../config
include ../configure-tools/Makefile.common
include ../stdlib/Makefile-stdlib.common

STDLIB_MLI=$(patsubst %.rmli,stdlib/%.mli,$(STDLIB_SRC)) stdlib/rml_async.mli

all: html

html: html/index.html

html/index.html: stdlib/pervasives.mli \
		$(STDLIB_MLI) $(patsubst %.mli,%.cmi,$(STDLIB_MLI))
	ocamldoc -I stdlib -hide-warnings \
		 -d html -html \
		 -sort -hide Rml_fake \
		 stdlib/pervasives.mli \
		 $(STDLIB_MLI)

clean:
	rm -f stdlib/pervasives.mli $(STDLIB_MLI) stdlib/*.cmi
realclean: clean
	rm -f html/*.html */*~ *~
cleanall: realclean

.PHONY:all html dvi clean realclean cleanall

# Import and compile stdmli as OCaml progrograms

stdlib/%.mli: ../stdlib/%.rmli stdlib/rml_fake.cmi
	echo "(* This file is generated *)" > $@
	echo "open Rml_fake;;" >> $@
	cat $< >> $@

stdlib/rml_async.mli: ../stdlib/thread/rml_async.rmli stdlib/rml_fake.cmi
	echo "(* This file is generated *)" > $@
	echo "open Rml_fake;;" >> $@
	cat $< >> $@

stdlib/%.cmi: stdlib/%.mli
	$(OCAMLC) -I stdlib $<


