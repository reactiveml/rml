RMLC=../../compiler/rmlc
RMLFLAGS= -dtypes -I ../../stdlib
N=-1
SAMPLING=-1.0
RUNTIME=ctrl_tree
INTERPRETERDIR=../../interpreter
INTERPRETER=rml_interpreter.cma
OCAMLC=ocamlc
OCAMLFLAGS=-dtypes

all : planetes-new

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -runtime $(RUNTIME) -sampling $(SAMPLING) -s $(MAIN) $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<

planetes-new.ml: planetes-new.rml rml_graphics.rzi
	$(RMLC) $(RMLFLAGS) -n $(N) -runtime $(RUNTIME) -sampling $(SAMPLING) -s systeme $< 
planetes-new.cmo: planetes-new.ml rml_graphics.cmo
planetes-new : planetes-new.cmo 
	 $(OCAMLC) $(FLAGS) -o planetes-new -I $(INTERPRETERDIR) unix.cma graphics.cma $(INTERPRETER) rml_graphics.cmo planetes-new.cmo 

rml_graphics.rzi: rml_graphics.rml
	$(RMLC) $(RMLFLAGS) -runtime $(RUNTIME) rml_graphics.rml

clean :
	rm -f *.cm* *.annot *.rzi *.rannot \
	      planetes-new planetes-new.ml \
	      rml_graphics.ml \
	      *~ 
