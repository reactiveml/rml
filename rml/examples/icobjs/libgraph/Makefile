PREFIX=
RMLC=rmlc
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
INTERPRETERPATH=`rmlc -where`
OCAMLOPTS=-I $(INTERPRETERPATH)



all:rml_Graphics.cma

opt:rml_Graphics.cmxa

rml_Graphics.cma:rml_Graphics.cmo misc.cmo
	$(OCAMLC) -I +camlimages -a -o rml_Graphics.cma ci_core.cma ci_graphics.cma ci_gif.cma misc.cmo rml_Graphics.cmo 

rml_Graphics.cmxa:rml_Graphics.cmx misc.cmx
	$(OCAMLOPT) -I +camlimages -a -o rml_Graphics.cmxa ci_core.a ci_graphics.a ci_gif.a misc.o rml_Graphics.o

clean:
	rm -f rml_Graphics.cmo rml_Graphics.ml misc.cmo rml_Graphics.cma
	rm -f *.rzi *.cmi

rml_Graphics.cmo: rml_Graphics.rml misc.rzi misc.cmi

rml_Graphics.cmx: rml_Graphics.rml misc.rzi misc.cmi

misc.cmo:misc.ml


misc.cmx:misc.ml

.SUFFIXES:
.SUFFIXES: .rml .ml .cmo .cmi .mli .rzi .rmli

.rml.ml:
	$(RMLC) $<

.ml.cmo:
	$(OCAMLC) -I +camlimages -c $(OCAMLOPTS) $<

.ml.cmx:
	$(OCAMLOPT) -I +camlimages -c $(OCAMLOPTS) $<

.mli.cmi:
	$(OCAMLC) -I +camlimages -c $(OCAMLOPTS) $<

.rmli.rzi:
	$(RMLC) $<

#LIBS = unix.cma graphics.cma -I +camlimages ci_core.cma ci_graphics.cma ci_bmp.cma $(INTERPRETERDIR)/$(INTERPRETER).cmo

