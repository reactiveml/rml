# File to compile
FILE=simul

# Reactive ML compiler
RMLC=../../../compiler/rmlc
N=-1
SAMPLING=-1.0
RUNTIME=ctrl_tree
RMLFLAGS=
MAIN=main

OCAMLC=ocamlc
OCAMLFLAGS=

OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=

EXEC = $(FILE)
EXECOPT = $(FILE).opt


all: byte opt

byte: $(EXEC)

opt: $(EXECOPT)



global.ml global.rzi: global.rml

pos_tbl.ml pos_tbl.rzi: pos_tbl.rml global.rzi 

area.ml area.rzi: area.rml global.rzi

simul.ml simul.rzi: simul.rml global.rzi pos_tbl.rzi area.rzi 
	$(RMLC) $(RMLFLAGS) -n $(N) -runtime $(RUNTIME) -sampling $(SAMPLING) -s $(MAIN) $<

simul.cmo: simul.ml global.cmo pos_tbl.cmo area.cmo 
simul: simul.cmo global.cmo pos_tbl.cmo area.cmo 
	$(OCAMLC) $(OCAMLFLAGS) -o simul unix.cma graphics.cma -I `$(RMLC) -where` rml_interpreter.cma global.cmo pos_tbl.cmo area.cmo simul.cmo 

simul.cmx: simul.ml global.cmx pos_tbl.cmx area.cmx 
simul.opt: simul.cmx global.cmx pos_tbl.cmx area.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o simul.opt unix.cmxa graphics.cmxa -I `$(RMLC) -where` rml_interpreter.cmxa global.cmx pos_tbl.cmx area.cmx simul.cmx 

# implicit rules
.SUFFIXES: .rml .mli .ml .cmi .cmo .cmx .o .rzi

.rml.rzi:
	$(RMLC) $(RMLFLAGS) -runtime $(RUNTIME) $<
.rml.ml:
	$(RMLC) $(RMLFLAGS) -runtime $(RUNTIME) $<

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


clean :
	rm -f *.cm* *.o *.annot *.rannot *.rzi \
	      simul.ml simul simul.opt \
	      global.ml area.ml pos_tbl.ml \
	      *~ 
