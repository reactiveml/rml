# File to compile
FILE=cellular_automata

# Reactive ML compiler
RMLC=rmlc
N=-1
SAMPLING=-1.0
RMLFLAGS=-dtypes
MAIN=main

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


caml_cellular_automata.cmo: caml_cellular_automata.ml
caml_cellular_automata: caml_cellular_automata.cmo 
	$(OCAMLC) $(OCAMLFLAGS) -o caml_cellular_automata unix.cma graphics.cma caml_cellular_automata.cmo 

caml_cellular_automata.cmx: caml_cellular_automata.ml
caml_cellular_automata.opt: caml_cellular_automata.cmx 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o caml_cellular_automata.opt unix.cmxa graphics.cmxa caml_cellular_automata.cmx 


# implicit rules
.SUFFIXES: .rml .mli .ml .cmi .cmo .cmx .o .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) -s $(MAIN) $<

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
		$(FILE).ml \
		cellular_automata_par.ml 

realclean: clean
	rm -f 	$(FILE) $(FILE).opt \
		cellular_automata_par cellular_automata_par.opt \
		caml_cellular_automata caml_cellular_automata.opt \
		*~
cleanall: realclean
