RMLC=../compiler/rmlc.byte -i
FILE=
FLAGS=
INTERPRETERDIR=../interpreter
INTERPRETER=$(INTERPRETERDIR)/rml_interpreter
OCAMLC=ocamlc
MAIN=main

all : $(FILE)

$(FILE).ml : $(FILE).rml
$(FILE).cmo : $(FILE).ml
$(FILE) : $(FILE).cmo $(INTERPRETER).cmo
	$(OCAMLC) $(FLAGS) -o $(FILE) unix.cma graphics.cma $(INTERPRETER).cmo $(FILE).cmo 

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .rzi

.rml.ml:
	$(RMLC) $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) $<


clean :
	rm -f *.cm* *.annot *.rzi $(FILE).ml $(FILE) *~ 
