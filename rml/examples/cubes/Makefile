OCAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = rmlc
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
	$(OCAMLC) -o $(EXEC) -I $(INTERPRETERPATH) unix.cma graphics.cma rml_interpreter.cma $(OBJS) 

$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) unix.cmxa graphics.cmxa rml_interpreter.cmxa $(OBJSOPT) 


cube.cmo: cube.ml moteur.cmi

main.cmo: main.ml moteur.cmi

moteur.cmo: moteur.ml


cube.cmx: cube.ml moteur.cmx

main.cmx: main.ml moteur.cmx

moteur.cmx: moteur.ml


main.ml: main.rml
	$(RMLC) -s main $<

clean:
	rm -f *.cmi *.cmo *.cmx cube.ml moteur.ml main.ml *.rzi *.o

cleanall:clean
	rm -f $(EXEC) $(EXECOPT)

realclean:cleanall

.PHONY: all byte opt clean cleanall realclean
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .rml .o






.rml.ml:
	$(RMLC) $<

.ml.cmo:
	$(OCAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmi:
	$(OCAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmx:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<

.ml.o:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<
