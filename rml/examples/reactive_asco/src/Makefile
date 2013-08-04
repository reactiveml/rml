RMLC=rmlc
RMLBUILD=rmlbuild -classic-display
FLAGS=-lib graphics \
	-lib str \
	-Is animations,applications,data_structures,input_output,ascolib \
	-cflags "-I `$(RMLC) -where`/toplevel" \
	-tag annot


TARGET=native

all: 	asco.rml.$(TARGET) \
	piano_phase.rml.$(TARGET) \
	frere_jacques.rml.$(TARGET) \
	babylon.rml.$(TARGET) \
	lib

asco.rml.$(TARGET):
	$(RMLBUILD) $(FLAGS) applications/$@
	cp _build/applications/$@ ..

piano_phase.rml.$(TARGET):
	$(RMLBUILD) $(FLAGS) applications/$@
	cp _build/applications/$@ ..

frere_jacques.rml.$(TARGET):
	$(RMLBUILD) $(FLAGS) applications/$@
	cp _build/applications/$@ ..

babylon.rml.$(TARGET):
	$(RMLBUILD) $(FLAGS) applications/$@
	cp _build/applications/$@ ..

# Ascolib
lib: ascolive.cma
	find _build -iname *.rzi -exec cp {} ../lib \;
	find _build -iname *.cmi -exec cp {} ../lib \;
	cp _build/applications/ascolive.cma ../lib

ascolive.cma:
	$(RMLBUILD) $(FLAGS) applications/$@


.SUFFIXES: .rml .rmli .ml .mli .rzi .cmo .cmi

%.cmo:
	$(RMLBUILD) $(FLAGS) $@

clean:
	$(RMLBUILD) -clean
	find ../lib -iname *.rzi -exec rm {} \;
	find ../lib -iname *.cmi -exec rm {} \;
	find ../lib -iname *.cma -exec rm {} \;

cleanall: clean
	find . -iname *~ -exec rm {} \;
	find ../lib -iname *~ -exec rm {} \;

realclean: cleanall

.PHONY: asco.rml.$(TARGET) clean cleanall
