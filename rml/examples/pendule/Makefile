CAMLC = ocamlc
OCAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
RMLC = ../../compiler/rmlc
INTERPRETERPATH=../../interpreter

SAMPLING = 0.02

RMLCOPTS = -I ../../stdlib


OBJS = moteur.cmo \
       pendule.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
          pendule.cmx \
          main.cmx

MLINTER = pendule.ml moteur.ml main.ml

EXEC    = pendule
EXECOPT = pendule.opt


LIBS = unix.cma graphics.cma rml_interpreter.cma

LIBSOPT = unix.cmxa graphics.cmxa rml_interpreter.cmxa



all: $(EXEC)


opt: $(EXECOPT)

clean :
	rm -f *.cmi *.cmo *.cmx *.o *.rzi
	rm -f $(MLINTER)
	rm -f *.rzi

cleanall : clean
	rm -f $(EXEC) $(EXECOPT)

$(EXEC): $(OBJS)
	$(CAMLC) -o $(EXEC) -I $(INTERPRETERPATH) $(LIBS) $(OBJS) 


$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) $(LIBSOPT) $(OBJSOPT) -cclib -lunix 


pendule.cmo: pendule.ml moteur.cmi

main.cmo: main.ml moteur.cmi pendule.cmi

pendule.cmx: pendule.ml moteur.cmi

main.cmx: main.ml moteur.cmx pendule.cmx

main.ml:main.rml
	$(RMLC) $(RMLCOPTS) -sampling $(SAMPLING) -s main $<




# Regles implicites :

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .rml .o


.rml.ml:
	$(RMLC) $(RMLCOPTS) $<

.ml.cmo:
	$(CAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmi:
	$(CAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmx:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(OCAMLOPT) -c $*.mli
	$(OCAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<


