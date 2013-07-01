OCAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = rmlc
INTERPRETERPATH=`$(RMLC) -where`
RMLCOPTS=


#  Ne pas modifier apres (et pendant) cette ligne

OBJS = spirale.cmo \


OBJSOPT = spirale.cmx \

EXEC = spirale
EXECOPT = spirale.opt


all:byte opt


byte: $(EXEC)


opt: $(EXECOPT)


$(EXEC): $(OBJS)
	$(OCAMLC) -o $(EXEC) -I $(INTERPRETERPATH) unix.cma graphics.cma rmllib.cma $(OBJS) 



$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -o $(EXECOPT) -I $(INTERPRETERPATH) unix.cmxa graphics.cmxa rmllib.cmxa $(OBJSOPT)  -cclib -lunix

spirale.cmo: spirale.ml 

spirale.cmx: spirale.ml 


spirale.ml: spirale.rml
	$(RMLC) $(RMLCOPTS) -s main $<



clean:
	rm -f *.cmi *.cmo *.cmx spirale.ml *.o *.rzi

cleanall: clean
	rm -f $(EXEC) $(EXECOPT)

realclean: cleanall

.PHONY: clean cleanall realclean all byte opt
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .rml

.rml.ml:
	$(RMLC) $(RMLCOPTS) $<

.ml.cmo:
	$(OCAMLC) -I $(INTERPRETERPATH) -c $<

.ml.cmi:
	$(OCAMLC) -I $(INTERPRETERPATH) -c $<

.ml.cmx:
	$(OCAMLOPT) -I $(INTERPRETERPATH) -c $<

