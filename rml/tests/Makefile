#RMLC=../compiler/rmlc.byte 
RMLC=rmlc
RMLFLAGS=-n 10
FILE=
FLAGS=
INTERPRETERDIR=../interpreter
INTERPRETER=$(INTERPRETERDIR)/lk_rewrite.cmo $(INTERPRETERDIR)/lco_ctrl_tree.cmo
OCAMLC=ocamlc
MAIN=""

all : $(FILE)

$(FILE).ml : $(FILE).rml
$(FILE).cmo : $(FILE).ml
$(FILE) : $(FILE).cmo 
	$(OCAMLC) $(FLAGS) -o $(FILE) unix.cma graphics.cma $(INTERPRETER) $(FILE).cmo 

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .rzi

.rml.ml:
	$(RMLC)  $(RMLFLAGS) -s $(MAIN) $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) $<


clean :
	rm -f *.cm* *.annot *.rzi $(FILE).ml $(FILE) *~ 
