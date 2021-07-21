RMLC=rmlc
RMLFLAGS=
N=-1
SAMPLING=-1.0
INTERPRETERDIR=`$(RMLC) -where`
INTERPRETER=rmllib.cma
OCAMLFIND=ocamlfind
OCAMLC=ocamlc
OCAMLFLAGS=

all : boids

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) -s $(MAIN) $<
.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -package graphics -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -package graphics -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<

boids.ml: boids.rml
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) -s main $<
boids.cmo: boids.ml
boids : boids.cmo
	 $(OCAMLFIND) $(OCAMLC) $(FLAGS) -o boids -I $(INTERPRETERDIR) -linkpkg -package graphics unix.cma $(INTERPRETER) boids.cmo

clean :
	rm -f *.cm* *.annot *.rzi \
	      boids.ml \
	      rml_graphics.ml 

realclean: clean
	rm -f boids *~ 
cleanall: realclean
