OCAMLFIND=ocamlfind
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
RMLC = rmlc

LUC4OCAML_DIR=`$(OCAMLC) -where`/lucky
# Pour mon ibook, commenter la ligne au dessus et decommentez celle d'en-dessous
#LUC4OCAML_DIR=`$(OCAMLC) -where`/luc4ocaml
# Pour vesuve
#LUC4OCAML_DIR=$(HOME)/Lucky/luc4ocaml-current/lib


## A modifier pour compiler sans ou avec Lucky

LIBPATH=-I `$(RMLC) -where`
LUC4OCAML_CMA =
LUC4OCAML_CMXA =

# LIBPATH=-I `$(RMLC) -where` -I $(LUC4OCAML_DIR)
# LUC4OCAML_CMA = luc4ocaml.cma
# LUC4OCAML_CMXA = str.cmxa bdd.cmxa polka.cmxa luc4ocaml.cmxa



RMLCFLAGS=
OCAMLCFLAGS=
OCAMLOPTFLAGS=

#  Ne pas modifier apres (et pendant) cette ligne


OBJS =	init.cmo \
	definitions.cmo \
	environment.cmo \
	rml_graphics.cmo \
	print.cmo \
	mac.cmo \
	routing.cmo \
	energy.cmo \
	graphic.cmo \
	application.cmo \
	stat.cmo \
	main.cmo

OBJSOPT = init.cmx \
	definitions.cmx \
	environment.cmx \
	rml_graphics.cmx \
	print.cmx \
        mac.cmx \
        routing.cmx \
        energy.cmx \
        graphic.cmx \
	application.cmx \
	stat.cmx \
        main.cmx




EXEC    = glonemo
EXECOPT = glonemo.opt


all: opt glonemo.cma


byte: $(EXEC)


opt: $(EXECOPT)

graphic.rzi: graphic.rml rml_graphics.rml

init.cmo: init.ml

definitions.cmo: init.cmo definitions.ml

environment.cmo: definitions.cmo environment.ml

main.cmo: main.ml definitions.cmi environment.cmi print.cmi mac.cmi routing.cmi energy.cmi graphic.cmi

print.cmo: print.ml definitions.cmi

mac.cmo: mac.ml definitions.cmi print.cmi

routing.cmo: routing.ml definitions.cmi

graphic.cmo: definitions.cmi graphic.ml

energy.cmo: energy.ml definitions.cmi

application.cmo: definitions.cmi routing.cmi application.ml


definitions.cmx: definitions.ml

init.cmx: init.ml

environment.cmx: definitions.cmx environment.ml

rml_graphics.cmx: rml_graphics.ml

main.cmx: main.ml definitions.cmi environment.cmi print.cmi mac.cmi routing.cmi energy.cmi graphic.cmi rml_graphics.cmi

print.cmx: print.ml definitions.cmi

mac.cmx: mac.ml definitions.cmi print.cmi

routing.cmx: routing.ml definitions.cmi

graphic.cmx: graphic.ml rml_graphics.ml definitions.cmi

energy.cmx: energy.ml definitions.cmi

application.cmx: application.ml definitions.cmi routing.cmi

main.ml:main.rml
	$(RMLC) -dtypes $(RMLCFLAGS) -s main $<
#$(RMLC) -s main -sampling -1 -n 100000 $(RMLCFLAGS)  $<




$(EXEC): $(OBJS)
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) \
		-o $(EXEC) $(LIBPATH) \
		-linkpkg -package graphics unix.cma rmllib.cma \
		$(LUC4OCAML_CMA) \
		$(OBJS)


$(EXECOPT): $(OBJSOPT)
	$(OCAMLFIND) $(OCAMLOPT) -o $(EXECOPT) $(OCAMLOPTFLAGS) \
		$(LIBPATH) \
		-linkpkg -package graphics unix.cmxa rmllib.cmxa\
		$(LUC4OCAML_CMXA) \
		$(OBJSOPT)


glonemo.cma: $(OBJS)
	$(RMLC) -dtypes $(RMLCFLAGS) main.rml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics $(OCAMLCFLAGS) $(LIBPATH) main.ml
	rm main.ml
	$(OCAMLFIND) $(OCAMLC) -a $(OCAMLCFLAGS) \
		-o glonemo.cma $(LIBPATH) \
		-linkpkg -package graphics \
		$(LUC4OCAML_CMA) \
		$(OBJS)



clean :
	-rm -f *.cmi *.cmo *.cmx *.ml *.o *.rzi *.sannot *.tannot

cleanall : clean
	-rm -f $(EXEC) $(EXECOPT) glonemo.cma *~

realclean : cleanall




#.PHONY: clean cleanall realclean $(EXECOPT) $(EXEC) all byte opt
#.SUFFIXES:
#.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .rml .o




%.ml: %.rml
	$(RMLC) $(RMLCFLAGS) -dtypes $<

%.rzi: %.rml
	$(RMLC) $(RMLCFLAGS) $<

%.cmi: %.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -c -package graphics $(LIBPATH) $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -c -package graphics $(LIBPATH) $<

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -c -package graphics $(LIBPATH) $<
