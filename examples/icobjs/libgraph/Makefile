PREFIX=
RMLC=rmlc
RMLFLAGS=
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
INTERPRETERPATH=`rmlc -where`
OCAMLOPTS=-I $(INTERPRETERPATH)
CAMLIMAGESDIR=-I +camlimages
# CAMLIMAGESDIR=-I $(HOME)/Ocaml/camlimages-2.2/ps \
# -I $(HOME)/Ocaml/camlimages-2.2/bmp \
# -I $(HOME)/Ocaml/camlimages-2.2/gif \
# -I $(HOME)/Ocaml/camlimages-2.2/png \
# -I $(HOME)/Ocaml/camlimages-2.2/ppm \
# -I $(HOME)/Ocaml/camlimages-2.2/xpm \
# -I $(HOME)/Ocaml/camlimages-2.2/jpeg \
# -I $(HOME)/Ocaml/camlimages-2.2/tiff \
# -I $(HOME)/Ocaml/camlimages-2.2/corelib \
# -I $(HOME)/Ocaml/camlimages-2.2/graphics \
# -I $(HOME)/Ocaml/camlimages-2.2/lablgtk \
# -I $(HOME)/Ocaml/camlimages-2.2/xvthumb \
# -I $(HOME)/Ocaml/camlimages-2.2/freetype2 \
# -I $(HOME)/Ocaml/camlimages-2.2/lablgtk2 \


all:rml_Graphics.cma

opt:rml_Graphics.cmxa

rml_Graphics.cma:rml_Graphics.cmo misc.cmo
	$(OCAMLC) $(CAMLIMAGESDIR) -a -o rml_Graphics.cma ci_core.cma ci_graphics.cma ci_gif.cma misc.cmo rml_Graphics.cmo 

rml_Graphics.cmxa:rml_Graphics.cmx misc.cmx
	$(OCAMLOPT) $(CAMLIMAGESDIR) -a -o rml_Graphics.cmxa ci_core.a ci_graphics.a ci_gif.a misc.o rml_Graphics.o

clean:
	rm -f rml_Graphics.cmo rml_Graphics.ml misc.cmo rml_Graphics.cma
	rm -f *.rzi *.cmi misc.mli

rml_Graphics.cmo: rml_Graphics.rml misc.rzi misc.cmi

rml_Graphics.cmx: rml_Graphics.rml misc.rzi misc.cmi

misc.cmo:misc.ml

misc.mli:misc.rmli

misc.cmx:misc.ml

.SUFFIXES:
.SUFFIXES: .rml .ml .cmo .cmi .mli .rzi .rmli

.rml.ml:
	$(RMLC) $(RMLFLAGS) $<

.ml.cmo:
	$(OCAMLC) $(CAMLIMAGESDIR) -c $(OCAMLOPTS) $<

.ml.cmx:
	$(OCAMLOPT) $(CAMLIMAGESDIR) -c $(OCAMLOPTS) $<

.mli.cmi:
	$(OCAMLC) $(CAMLIMAGESDIR) -c $(OCAMLOPTS) $<

.rmli.rzi:
	$(RMLC) $(RMLFLAGS) $<

#LIBS = unix.cma graphics.cma $(CAMLIMAGESDIR) ci_core.cma ci_graphics.cma ci_bmp.cma $(INTERPRETERDIR)/$(INTERPRETER).cmo

