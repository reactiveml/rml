# File to compile
FILE=rogervilder_carres_noir_et_blanc ala_rogervilder

# ReactiveML compiler
RMLC=rmlc -runtime Lco_n
N=-1
SAMPLING=0.01

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
	$(OCAMLC) $(OCAMLFLAGS) -o $(FILE) unix.cma graphics.cma -I `$(RMLC) -where` rmllib.cma $(FILE).cmo

$(FILE).cmx: $(FILE).ml
$(FILE).opt: $(FILE).cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(FILE).opt unix.cmxa graphics.cmxa -I `$(RMLC) -where` rmllib.cmxa $(FILE).cmx

# implicit rules
.SUFFIXES: .rml .mli .ml .cmi .cmo .cmx .o .rzi

.rml.ml:
	$(RMLC) -n $(N) -sampling $(SAMPLING) $<

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
	rm -f *.cm* *.annot *.rzi $(FILE).ml *.o

realclean: clean
	rm -f $(FILE) $(FILE).opt *~
cleanall: realclean
