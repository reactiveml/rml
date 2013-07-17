RMLC=rmlc
RMLFLAGS=
N=-1
SAMPLING=0.01
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

planets.opt : planets.cmx
	 $(OCAMLOPT) $(FLAGS) -o planets.opt -I $(INTERPRETERDIR) unix.cmxa graphics.cmxa $(INTERPRETERX) planets.cmx

clean :
	rm -f *.cm* *.annot *.rzi *.?annot *.o \
	      planets.ml \

realclean: clean
	rm -f planets planets.opt *~
cleanall: realclean

# implicit rules
.SUFFIXES : .rml .mli .ml .cmi .cmo .cmx .rzi

.rml.ml:
	$(RMLC) $(RMLFLAGS) -n $(N) -sampling $(SAMPLING) $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<

