CAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = rmlc
INTERPRETER=rml_interpreter
INTERPRETERDIR=`$(RMLC) -where`


# Options :
RMLOPTS =
OCAMLOPTS = -I $(INTERPRETERDIR) 



OBJS = moteur.cmo \
       calcul.cmo \
       balle.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
          calcul.cmx \
          balle.cmx \
          main.cmx

MLINTER = balle.ml moteur.ml main.ml calcul.ml

EXEC    = balle
EXECOPT = $(EXEC).opt


LIBS = unix.cma graphics.cma $(INTERPRETERDIR)/$(INTERPRETER).cma

LIBSOPT = unix.cmxa graphics.cmxa $(INTERPRETERDIR)/$(INTERPRETER).cmxa



all: byte opt

byte: $(EXEC)


opt: $(EXECOPT)

clean :
	rm -f *.cmi *.cmo *.cmx *.o
	rm -f $(MLINTER)
	rm -f *.rzi

realclean: cleanall

cleanall: clean
	rm -f rubik.ml moteur.ml main.ml interpreter.cmi $(EXEC) $(EXECOPT)

$(EXEC): $(OBJS)
	$(CAMLC) -o $(EXEC) $(LIBS) $(OBJS) 


$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) $(LIBSOPT) $(OBJSOPT) -cclib -lunix 


balle.ml: calcul.rzi moteur.rzi

balle.cmo balle.cmi: balle.ml calcul.cmi moteur.cmi








main.cmo: main.ml moteur.cmi balle.cmi

main.cmx: main.ml moteur.cmx balle.cmx

main.ml:main.rml
	$(RMLC) -s main $<


moteur.cmi moteur.cmo: moteur.ml
	$(CAMLC)  -I $(INTERPRETERDIR) -c $<

balle.cmx: balle.ml calcul.cmx

# Regles implicites :
.PHONY: clean cleanall all opt byte realclean
.SUFFIXES: .ml .mli .cmo .cmi .cmx .rml .rzi


.rml.ml:
	$(RMLC) $(RMLOPTS) $<

.rml.rzi:
	$(RMLC) $(RMLOPTS) $<


.ml.cmo:
	$(CAMLC) -c $(OCAMLOPTS) $<

.ml.cmi:
	$(CAMLC) -c $(OCAMLOPTS) $< 

.ml.cmx:
	$(OCAMLOPT) -c $(OCAMLOPTS) $<



