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

all: rmltop

lib:
	rmlc -c rmltop_global.rmli
	ocamlc  -I `rmlc -where` rmltop_global.mli
	ocamlc -c -I `rmlc -where` -thread rmltop_global.ml
	ocamlc -c -I `rmlc -where` -thread rmltop_implantation.ml
	rmlc -runtime Rmltop rmltop_machine_controler.rml 
	ocamlc -c -I `rmlc -where` -thread rmltop_machine_controler.ml
	ocamlc -c -thread rmltop_directives.ml
	ocamlc -c -thread rmltop_main.ml

rmltop: lib
	ocamlc -o rmltop -I +threads -I `rmlc -where` unix.cma str.cma \
		threads.cma \
		rml_interpreter.cma \
		rmltop_global.cmo \
		rmltop_directives.cmo \
		rmltop.ml

install: install.lib install.bin

install.bin: rmltop
	$(INSTALL) -d $(bindir)
	$(INSTALL) rmltop $(bindir)/rmltop

install.lib: $(INSTALLFILE)
	$(INSTALL) -d $(LIBDIR)
	$(INSTALL) -m 644 $(INSTALLFILE) $(LIBDIR)


test:
	ledit ocaml -I +threads -I `rmlc -where` unix.cma threads.cma \
		rml_interpreter.cma \
		rmltop_global.cmo \
		rmltop_implantation.cmo \
		rmltop_machine_controler.cmo \
		rmltop_directives.cmo \
		rmltop_main.cmo
clean: 
	rm -f *.cm* *.rzi 

realclean: clean
	rm -f *~ rmltop_machine_controler.ml *.annot *.rannot rmltop

cleanall: realclean

