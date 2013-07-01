RMLC=rmlc
RMLFLAGS=
N=-1
SAMPLING=-1.0
INTERPRETERDIR=`$(RMLC) -where`
INTERPRETER=rmllib.cma
OCAMLC=ocamlc
OCAMLFLAGS=

all : boids

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) -s $(MAIN) $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<

boids.ml: boids.rml 
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) -s main $< 
boids.cmo: boids.ml 
boids : boids.cmo 
	 $(OCAMLC) $(FLAGS) -o boids -I $(INTERPRETERDIR) unix.cma graphics.cma $(INTERPRETER) boids.cmo 

clean :
	rm -f *.cm* *.annot *.rzi \
	      boids.ml \
	      rml_graphics.ml 

realclean: clean
	rm -f boids *~ 
cleanall: realclean
