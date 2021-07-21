RMLC=rmlc
RMLFLAGS=
N=-1
SAMPLING=0.01
INTERPRETERDIR=`rmlc -where`
INTERPRETER=rmllib.cma
INTERPRETERX=rmllib.cmxa
OCAMLFIND=ocamlfind
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLFLAGS=
MAIN=systeme

all : planets
opt : planets.opt

planets : planets.cmo
	 $(OCAMLFIND) $(OCAMLC) $(FLAGS) -o planets -I $(INTERPRETERDIR) -linkpkg -package graphics unix.cma $(INTERPRETER) planets.cmo

planets.opt : planets.cmx
	 $(OCAMLFIND) $(OCAMLOPT) $(FLAGS) -o planets.opt -I $(INTERPRETERDIR) -linkpkg -package graphics unix.cmxa $(INTERPRETERX) planets.cmx

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
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) -package graphics $(INCLUDES) $<

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) -package graphics $(INCLUDES) $<

.mli.cmi:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c -I $(INTERPRETERDIR) -package graphics $(INCLUDES) $<

.mli.rzi:
	$(RMLC) -c $<

