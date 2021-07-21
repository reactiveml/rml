CAMLC = ocamlfind ocamlc -package graphics
OCAMLOPT = ocamlfind ocamlopt -package graphics
RMLC = rmlc
INTERPRETER=rmllib
INTERPRETERDIR=`$(RMLC) -where`


SAMPLING=0.01


# Options :
RMLOPTS =-I libgraph
OCAMLOPTS = -I $(INTERPRETERDIR) -I libgraph



OBJS = icobj.cmo \
       behavior.cmo \
       entities.cmo \
       scene.cmo \
       main.cmo

OBJSOPT = icobj.cmx \
       behavior.cmx \
       entities.cmx \
          scene.cmx \
          main.cmx

MLINTER = icobj.ml scene.ml main.ml entities.ml behavior.ml

EXEC    = icobj
EXECOPT = icobj.opt


LIBS = unix.cma graphics.cma $(INTERPRETERDIR)/$(INTERPRETER).cma

LIBSOPT = unix.cmxa graphics.cmxa $(INTERPRETERDIR)/$(INTERPRETER).cmxa



all: $(EXEC)


opt: $(EXECOPT)

clean :
	rm -f *.cmi *.cmo *.cmx *.o
	rm -f $(MLINTER)
	rm -f *.rzi *.?annot

cleanall : clean
	rm -f $(EXEC) $(EXECOPT)
realclean: cleanall

$(EXEC): $(OBJS)
	$(CAMLC) -linkpkg -o $(EXEC) $(OCAMLOPTS) $(LIBS) rml_Graphics.cma $(OBJS)


$(EXECOPT): $(OBJSOPT)
	$(OCAMLOPT) -linkpkg -o $(EXECOPT) $(LIBSOPT) rml_Graphics.cmxa $(OBJSOPT)  -cclib -lunix


scene.ml: icobj.cmo entities.cmo

scene.cmo scene.cmi: scene.ml


main.cmo: main.ml scene.cmi

main.cmx: main.ml scene.cmx

main.ml:main.rml
	$(RMLC) $(RMLOPTS) -s main -sampling $(SAMPLING) $<


.PHONY: all opt clean cleanall

# Regles implicites :
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .rml .rzi


.rml.ml:
	$(RMLC) $(RMLOPTS) $<

.rml.rzi:
	$(RMLC) $(RMLOPTS) $<


.ml.cmo:
	$(CAMLC) -I libgraph -c $(OCAMLOPTS) $<

.ml.cmi:
	$(CAMLC) -I libgraph -c $(OCAMLOPTS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OCAMLOPTS) $<
