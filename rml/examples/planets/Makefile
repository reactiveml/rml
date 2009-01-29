RMLC=rmlc
RMLFLAGS=
N=10000
SAMPLING=0
INTERPRETERDIR=`rmlc -where`
INTERPRETER=rmllib.cma
INTERPRETERX=rmllib.cmxa
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLFLAGS=
MAIN=systeme

all : planets
opt : planets.opt

planets : planets.cmo 
	 $(OCAMLC) $(FLAGS) -o planets -I $(INTERPRETERDIR) unix.cma graphics.cma $(INTERPRETER) planets.cmo 

planets : planets.cmx 
	 $(OCAMLOPT) $(FLAGS) -o planets.opt -I $(INTERPRETERDIR) unix.cmxa graphics.cmxa $(INTERPRETERX) planets.cmx 

clean :
	rm -f *.cm* *.annot *.rzi *.?annot \
	      planets.ml \

realclean: clean
	rm -f planets *~
cleanall: realclean

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .cmx .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -s $(MAIN) $<
# -sampling $(SAMPLING) 
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<

