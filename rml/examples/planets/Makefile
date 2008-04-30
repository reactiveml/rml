RMLC=rmlc
RMLFLAGS=
N=-1
SAMPLING=0.01
INTERPRETERDIR=`rmlc -where`
INTERPRETER=rmllib.cma
OCAMLC=ocamlc
OCAMLFLAGS=
MAIN=systeme

all : planets


planets : planets.cmo 
	 $(OCAMLC) $(FLAGS) -o planets -I $(INTERPRETERDIR) unix.cma graphics.cma $(INTERPRETER) planets.cmo 

clean :
	rm -f *.cm* *.annot *.rzi *.?annot \
	      planets.ml \

realclean: clean
	rm -f planets *~
cleanall: realclean

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

