OCAMLC = ocamlc
OCAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
RMLC = ../../compiler/rmlc
INTERPRETER=interpreter
INTERPRETERPATH=../../interpreter
RMLCOPTS= -I ../../stdlib

OBJS = moteur.cmo \
       cubes.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
       cubes.cmx \
       main.cmx


EXEC = cubes
EXECOPT = cubes.opt


all: byte opt

byte: $(EXEC)

opt: $(EXECOPT)

$(EXEC): $(OBJS)
	$(OCAMLC) -o $(EXEC) -I $(INTERPRETERPATH) unix.cma graphics.cma rml_interpreter.cma $(OBJS) 

$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) unix.cmxa graphics.cmxa rml_interpreter.cmxa $(OBJSOPT) 


cubes.cmo: cubes.ml moteur.cmi

main.cmo: main.ml moteur.cmi

moteur.cmo: moteur.ml


cubes.cmx: cubes.ml moteur.cmx

main.cmx: main.ml moteur.cmx

moteur.cmx: moteur.ml

main.ml: main.rml
	$(RMLC) $(RMLCOPTS) -s systeme $<

clean:
	rm -f *.cmi *.cmo *.cmx cubes.ml moteur.ml main.ml *.rzi *.o

cleanall:clean
	rm -f $(EXEC) $(EXECOPT)

realclean:cleanall

.PHONY: all byte opt clean cleanall realclean
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .rml .o






.rml.ml:
	$(RMLC) $(RMLCOPTS) $<

.ml.cmo:
	$(OCAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmi:
	$(OCAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmx:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<

.ml.o:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<
