CAMLC = ocamlc
OCAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
RMLC = ../../compiler/rmlc
INTERPRETER=rml_interpreter
INTERPRETERDIR=../../interpreter


# Options :
RMLOPTS = -I ../../stdlib
OCAMLOPTS = -I $(INTERPRETERDIR) 



OBJS = moteur.cmo \
       calcul.cmo \
       balle.cmo \
       main.cmo

OBJSOPT = moteur.cmx \
          balle.cmx \
          main.cmx

MLINTER = balle.ml moteur.ml main.ml calcul.ml

EXEC    = balle
EXECOPT = balleopt


LIBS = unix.cma graphics.cma $(INTERPRETERDIR)/$(INTERPRETER).cma

LIBSOPT = unix.cmxa graphics.cmxa $(INTERPRETERDIR)/$(INTERPRETER).cmxa



all: $(EXEC)


opt: $(EXECOPT)

clean :
	rm -f *.cmi *.cmo *.cmx *.o
	rm -f $(MLINTER)
	rm -f *.rzi

cleanall : clean
	rm -f $(EXEC) $(EXECOPT)

$(EXEC): $(OBJS)
	$(CAMLC) -o $(EXEC) $(LIBS) $(OBJS) 


$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) $(LIBSOPT) $(OBJSOPT) -cclib -lunix 


balle.ml: calcul.rzi moteur.rzi

balle.cmo balle.cmi: balle.ml calcul.cmi moteur.cmi








main.cmo: main.ml moteur.cmi balle.cmi

main.cmx: main.ml moteur.cmx balle.cmx

main.ml:main.rml
	echo opts = $(RMLOPTS)
	$(RMLC) $(RMLOPTS) -s main $<


moteur.cmi moteur.cmo: moteur.ml
	$(CAMLC)  -I $(INTERPRETERDIR) -c $<



# Regles implicites :

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



