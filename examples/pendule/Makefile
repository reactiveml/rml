CAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = rmlc
INTERPRETERPATH=`$(RMLC) -where`

SAMPLING = 0.02

RMLCOPTS =


OBJS = moteur.cmo \
       pendule.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
          pendule.cmx \
          main.cmx

MLINTER = pendule.ml moteur.ml main.ml

EXEC    = pendule
EXECOPT = pendule.opt


LIBS = unix.cma graphics.cma rmllib.cma

LIBSOPT = unix.cmxa graphics.cmxa rmllib.cmxa



all: $(EXEC)


opt: $(EXECOPT)

clean :
	rm -f *.cmi *.cmo *.cmx *.o *.rzi
	rm -f $(MLINTER)
	rm -f *.rzi

cleanall : clean
	rm -f $(EXEC) $(EXECOPT)
realclean: cleanall

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

.SUFFIXES: .ml .mli .cmo .cmi .cmx .rml .o


.rml.ml:
	$(RMLC) $(RMLCOPTS) $<

.ml.cmo:
	$(CAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmi:
	$(CAMLC) -c -I $(INTERPRETERPATH) $<

.ml.cmx:
	$(OCAMLOPT) -c -I $(INTERPRETERPATH) $<



