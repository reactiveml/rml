OCAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = ../../compiler/rmlc
INTERPRETER = interpreter
INTERPRETERPATH=../../interpreter
INTERPRETERTYPE=ctrl_tree
RMLCOPTS= -I ../../stdlib


#  Ne pas modifier apres (et pendant) cette ligne

ifeq ($(INTERPRETERTYPE),ctrl_tree)
INTERTYPE = lco_ctrl_tree
else
INTERTYPE = lk_rewrite
endif

OBJS = moteur.cmo \
       spirale.cmo \


OBJSOPT = moteur.cmx \
       spirale.cmx \

EXEC = spirale
EXECOPT = spirale.opt


all:byte opt


byte: $(EXEC)


opt: $(EXECOPT)


$(EXEC): $(OBJS)
	$(OCAMLC) -o $(EXEC) -I $(INTERPRETERPATH) unix.cma graphics.cma $(INTERTYPE).cmo $(OBJS) 



$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) unix.cmxa graphics.cmxa $(INTERTYPE).cmx $(OBJSOPT)  -cclib -lunix

spirale.cmo: spirale.ml moteur.cmi

moteur.cmo: moteur.ml


spirale.cmx: spirale.ml moteur.cmi

moteur.cmx: moteur.ml


spirale.ml: spirale.rml
	$(RMLC) $(RMLCOPTS) -s main -runtime $(INTERPRETERTYPE) $<

moteur.ml: moteur.rml



clean:
	rm -f *.cmi *.cmo *.cmx spirale.ml moteur.ml *.o *.rzi

cleanall: clean
	rm -f $(EXEC) $(EXECOPT)

realclean: cleanall

.PHONY: clean cleanall realclean all byte opt $(EXEC) $(EXECOPT)
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .rml

.rml.ml:
	$(RMLC) $(RMLCOPTS) -runtime $(INTERPRETERTYPE) $<

.ml.cmo:
	$(OCAMLC) -I $(INTERPRETERPATH) -c $<

.ml.cmi:
	$(OCAMLC) -I $(INTERPRETERPATH) -c $<

.ml.cmx:
	$(OCAMLOPT) -I $(INTERPRETERPATH) -c $<

