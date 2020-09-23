OCAMLFIND=ocamlfind
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = rmlc
RMLFLAGS=
INTERPRETER=interpreter
INTERPRETERPATH=`$(RMLC) -where`


OBJS = moteur.cmo \
       cube.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
       cube.cmx \
       main.cmx


EXEC = cube
EXECOPT = cube.opt


all: byte opt

byte: $(EXEC)

opt: $(EXECOPT)

$(EXEC): $(OBJS)
	$(OCAMLFIND) $(OCAMLC) -o $(EXEC) -I $(INTERPRETERPATH) -linkpkg -package graphics unix.cma rmllib.cma $(OBJS)

$(EXECOPT): $(OBJSOPT)
	$(OCAMLFIND) $(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) -linkpkg -package graphics unix.cmxa rmllib.cmxa $(OBJSOPT)


cube.cmo: cube.ml moteur.cmi

main.cmo: main.ml moteur.cmi

moteur.cmo: moteur.ml


cube.cmx: cube.ml moteur.cmx

main.cmx: main.ml moteur.cmx

moteur.cmx: moteur.ml


main.ml: main.rml
	$(RMLC) $(RMLFLAGS) -s main $<

clean:
	rm -f *.cmi *.cmo *.cmx cube.ml moteur.ml main.ml *.rzi *.o

cleanall:clean
	rm -f $(EXEC) $(EXECOPT)

realclean:cleanall

.PHONY: all byte opt clean cleanall realclean
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .rml .o






.rml.ml:
	$(RMLC) $(RMLFLAGS) $<

.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I $(INTERPRETERPATH) $<

.ml.cmi:
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I $(INTERPRETERPATH) $<

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I $(INTERPRETERPATH) $<

.ml.o:
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I $(INTERPRETERPATH) $<
