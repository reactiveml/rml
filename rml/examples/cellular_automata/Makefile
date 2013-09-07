# File to compile
FILE=cellular_automata

# ReactiveML compiler
RMLC=rmlc
N=-1
SAMPLING=-1.0
RMLFLAGS=-dtypes

OCAMLC=ocamlc
OCAMLFLAGS=

OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=

EXEC = $(FILE)
EXECOPT = $(FILE).opt


all: opt

byte: $(EXEC)

opt: $(EXECOPT)

$(FILE).ml: $(FILE).rml

$(FILE).cmo: $(FILE).ml
$(FILE): $(FILE).cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $(FILE) unix.cma graphics.cma -I `$(RMLC) -where` rmllib.cma $(FILE).cmo

$(FILE).cmx: $(FILE).ml
$(FILE).opt: $(FILE).cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(FILE).opt unix.cmxa graphics.cmxa -I `$(RMLC) -where` rmllib.cmxa $(FILE).cmx


# implicit rules
.SUFFIXES: .rml .mli .ml .cmi .cmo .cmx .o .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I `$(RMLC) -where` $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I `$(RMLC) -where` $(INCLUDES) $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I `$(RMLC) -where` $(INCLUDES) $<

.ml.o:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -I `$(RMLC) -where` $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<


clean:
	rm -f *.cm* *.o *.annot *.?annot *.rzi \
		$(FILE).ml

realclean: clean
	rm -f $(FILE) $(FILE).opt *~
cleanall: realclean
