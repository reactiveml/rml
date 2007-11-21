include ../config

INSTALLFILE=	rmltop.cmi \
		rmltop_global.cmi \
		rmltop_machine_controler.cmi \
		rmltop.cmo \
		rmltop_global.cmo \
		rmltop_machine_controler.cmo \
		rmltop_directives.cmi \
		rmltop_implantation.cmi \
		rmltop_main.cmi \
		rmltop_directives.cmo \
		rmltop_implantation.cmo \
		rmltop_main.cmo

RMLC=../compiler/rmlc
RMLLIB=-I ../stdlib -I ../interpreter

all: rmltop

lib: $(RMLC)
	$(RMLC) $(RMLLIB) -c rmltop_global.rmli
	$(OCAMLC) $(RMLLIB) rmltop_global.mli
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_global.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_implantation.ml
	$(RMLC) $(RMLLIB) -runtime Rmltop rmltop_machine_controler.rml 
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_machine_controler.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_directives.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_main.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop.ml

rmltop: lib
	$(OCAMLC) -o rmltop -I +threads $(RMLLIB) unix.cma str.cma \
		threads.cma \
		rmllib.cma \
		rmltop_global.cmo \
		rmltop_directives.cmo \
		rmltop.cmo

$(RMLC): 
	(cd ../compiler/; make $(TARGET))

install: install.lib install.bin

install.bin: rmltop
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) rmltop $(BINDIR)/rmltop

install.lib: lib 
	$(INSTALL) -d $(LIBDIR)
	$(INSTALL) -m 644 $(INSTALLFILE) $(LIBDIR)


test:
	ledit ocaml -I +threads $(RMLLIB) unix.cma threads.cma \
		rmllib.cma \
		rmltop_global.cmo \
		rmltop_implantation.cmo \
		rmltop_machine_controler.cmo \
		rmltop_directives.cmo \
		rmltop_main.cmo

wc:
	wc -l *.ml *.mli *.rml *.rmli

clean: 
	rm -f *.cm* *.rzi 

realclean: clean
	rm -f *~ rmltop_machine_controler.ml *.annot *.?annot rmltop

cleanall: realclean

FORCE: