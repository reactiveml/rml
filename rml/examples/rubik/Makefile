CAMLC = ocamlc
OCAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
RMLC = ../../compiler/rmlc
INTERPRETER=interpreter
INTERPRETERPATH=../../interpreter
INTERPRETERTYPE=ctrl_tree
RMLCOPTS= -I ../../stdlib
# INTERPRETERTYPE=rewrite


#  Ne pas modifier apres (et pendant) cette ligne 

OBJS = moteur.cmo \
       rubik.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
          rubik.cmx \
          main.cmx

EXEC    = rubik
EXECOPT = rubik.opt


all: byte opt


byte: $(EXEC)


opt: $(EXECOPT)


$(EXEC): $(OBJS)
	$(CAMLC) -o $(EXEC) -I  $(INTERPRETERPATH) unix.cma graphics.cma rml_interpreter.cma $(OBJS) 


$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) unix.cmxa graphics.cmxa rml_interpreter.cmxa $(OBJSOPT) -cclib -lunix 

main.ml:main.rml
	$(RMLC) $(RMLCOPTS) -s systeme -runtime $(INTERPRETERTYPE) $<


rubik.cmo: rubik.ml moteur.cmi

main.cmo: main.ml moteur.cmi rubik.cmi

moteur.cmo: moteur.ml


rubik.cmx: rubik.ml moteur.cmx

main.cmx: main.ml moteur.cmx rubik.cmx

moteur.cmx: moteur.ml


clean :
	-rm -f *.cmi *.cmo *.cmx rubik.ml moteur.ml main.ml *.o *.rzi

cleanall : clean
	-rm -f $(EXEC) $(EXECOPT)

realclean : cleanall




.PHONY: clean cleanall realclean $(EXECOPT) $(EXEC) all byte opt
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .rml .o




.ml.o:
	$(OCAMLOPT) -c $<

.rml.ml:
	$(RMLC) $(RMLCOPTS) -runtime $(INTERPRETERTYPE) $<

.ml.cmo:
	$(CAMLC) -c -I ../../interpreter $<

.ml.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<
