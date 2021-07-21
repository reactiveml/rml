# File to compile
FILE=reflex

# ReactiveML compiler
RMLC=rmlc
N=-1
SAMPLING=-1.0
RMLFLAGS=

OCAMLFIND=ocamlfind
OCAMLC=ocamlc
OCAMLFLAGS=

OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=

EXEC = $(FILE)
EXECOPT = $(FILE).opt


all: byte opt

byte: $(EXEC)

opt: $(EXECOPT)

$(FILE).ml: $(FILE).rml

$(FILE).cmo: $(FILE).ml
$(FILE): $(FILE).cmo
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -o $(FILE) -linkpkg -package graphics unix.cma -I `$(RMLC) -where` rmllib.cma $(FILE).cmo

$(FILE).cmx: $(FILE).ml
$(FILE).opt: $(FILE).cmx
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(FILE).opt -linkpkg -package graphics unix.cmxa -I `$(RMLC) -where` rmllib.cmxa $(FILE).cmx

# implicit rules
.SUFFIXES: .rml .mli .ml .cmi .cmo .cmx .o .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) $<

.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -package graphics -I `$(RMLC) -where` $(INCLUDES) $<

.mli.cmi:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -package graphics -I `$(RMLC) -where` $(INCLUDES) $<

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -c -package graphics -I `$(RMLC) -where` $(INCLUDES) $<

.ml.o:
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -c -package graphics -I `$(RMLC) -where` $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<


clean:
	rm -f *.cm* *.annot *.rzi $(FILE).ml *.o

realclean: clean
	rm -f $(FILE) $(FILE).opt *~
cleanall: realclean
