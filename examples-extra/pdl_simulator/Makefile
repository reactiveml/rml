# File to compile
FILE=simul

# Reactive ML compiler
RMLC=rmlc
N=-1
SAMPLING=-1.0
RMLFLAGS=
MAIN=main

OCAMLC=ocamlfind ocamlc -package graphics
OCAMLFLAGS=

OCAMLOPT=ocamlfind ocamlopt -package graphics
OCAMLOPTFLAGS=

EXEC = $(FILE)
EXECOPT = $(FILE).opt


all: byte opt

byte: $(EXEC)

opt: $(EXECOPT)



global.ml global.rzi: global.rml

rml_graphics.ml rml_graphics.rzi: rml_graphics.rml

draw.ml draw.rzi: draw.rml global.rzi rml_graphics.rzi
draw.cmo: draw.ml rml_graphics.cmo
draw.cmx: draw.ml rml_graphics.cmx

pos_tbl.ml pos_tbl.rzi: pos_tbl.rml global.rzi

area.ml area.rzi: area.rml global.rzi

move.ml move.rzi: move.rml global.rzi

msg.ml msg.rzi: msg.rml global.rzi

routage.ml routage.rzi: routage.rml global.rzi draw.rzi stat.rzi

routage.cmo: routage.ml draw.cmo stat.cmo
routage.cmx: routage.ml draw.cmx stat.cmx

stat.ml stat.rzi: stat.rml global.rzi

simul.ml simul.rzi: simul.rml global.rzi pos_tbl.rzi area.rzi move.rzi msg.rzi routage.rzi draw.rzi stat.rzi
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) -s $(MAIN) $<

simul.cmo: simul.ml global.cmo pos_tbl.cmo area.cmo move.cmo msg.cmo routage.cmo draw.cmo stat.cmo
simul: simul.cmo global.cmo pos_tbl.cmo area.cmo move.cmo msg.cmo routage.cmo draw.cmo stat.cmo
	$(OCAMLC) $(OCAMLFLAGS) -linkpkg -o simul unix.cma graphics.cma -I `$(RMLC) -where` rmllib.cma global.cmo rml_graphics.cmo draw.cmo stat.cmo pos_tbl.cmo area.cmo move.cmo msg.cmo routage.cmo simul.cmo

simul.cmx: simul.ml global.cmx pos_tbl.cmx area.cmx move.cmx msg.cmx routage.cmx draw.cmx stat.cmx
simul.opt: simul.cmx global.cmx pos_tbl.cmx area.cmx move.cmx msg.cmx routage.cmx draw.cmx stat.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkpkg -o simul.opt unix.cmxa graphics.cmxa -I `$(RMLC) -where` rmllib.cmxa global.cmx rml_graphics.cmx draw.cmx stat.cmx pos_tbl.cmx area.cmx move.cmx msg.cmx routage.cmx simul.cmx

# implicit rules
.SUFFIXES: .rml .mli .ml .cmi .cmo .cmx .o .rzi

.rml.rzi:
	$(RMLC) $(RMLFLAGS) $<
.rml.ml:
	$(RMLC) $(RMLFLAGS) $<

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
	      simul.ml \
	      global.ml area.ml pos_tbl.ml routage.ml move.ml msg.ml draw.ml \
	      stat.ml rml_graphics.ml

realclean: clean
	rm -f simul simul.opt *~
cleanall: realclean
