CAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = rmlc
INTERPRETER=interpreter
INTERPRETERPATH=`$(RMLC) -where`

RMLCOPTS=

#  Ne pas modifier apres (et pendant) cette ligne 


OBJS = moteur.cmo \
       rubik.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
          rubik.cmx \
          main.cmx

EXEC    = rubik
EXECOPT = rubik.opt


all: byte


byte: $(EXEC)


opt: $(EXECOPT)


$(EXEC): $(OBJS)
	$(CAMLC) -o $(EXEC) -I  $(INTERPRETERPATH) unix.cma graphics.cma rml_interpreter.cma $(OBJS) 


$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) unix.cmxa graphics.cmxa rml_interpreter.cmxa $(OBJSOPT) -cclib -lunix 

main.ml:main.rml
	$(RMLC) -s main $(RMLCOPTS)  $<


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
	$(RMLC) $(RMLCOPTS)  $<

.ml.cmo:
	$(CAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<
