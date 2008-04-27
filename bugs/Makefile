RMLC=rmlc
FILE=
FLAGS=
RMLCFLAGS=
INTERPRETERDIR=`rmlc -where`
INTERPRETER=rmllib
OCAMLC=ocamlc
MAIN=main

all : $(FILE)

$(FILE).ml : $(FILE).rml
$(FILE).cmo : $(FILE).ml
$(FILE) : $(FILE).cmo 
	$(OCAMLC) $(FLAGS) -o $(FILE) unix.cma graphics.cma -I $(INTERPRETERDIR) $(INTERPRETER).cma $(FILE).cmo 

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .rzi

.rml.ml:
	$(RMLC) $(RMLCFLAGS) $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) $<


clean :
	rm -f *.cm* *.annot *.rzi $(FILE).ml $(FILE) *~ 
