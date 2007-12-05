include ../config

INSTALLFILE=	rmltop.cmi \
		rmltop_global.cmi \
		rmltop_machine_body.cmi \
		rmltop_reactive_machine.cmi \
		rmltop_machine_controller.cmi \
		rmltop_lexer.cmo \
		rmltop.cmo \
		rmltop_global.cmo \
		rmltop_machine_body.cmo \
		rmltop_reactive_machine.cmo \
		rmltop_machine_controller.cmo \
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
	$(OCAMLLEX) rmltop_lexer.mll
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_lexer.ml
	$(RMLC) -c $(RMLLIB) rmltop_global.rmli
	$(OCAMLC) $(RMLLIB) rmltop_global.mli
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_global.ml
	$(RMLC) $(RMLLIB) -runtime Lco_ctrl_tree rmltop_machine_body.rml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_machine_body.ml
	$(RMLC) $(RMLLIB) rmltop_reactive_machine.rmli
	$(OCAMLC) $(RMLLIB) rmltop_reactive_machine.mli
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_reactive_machine.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_implantation.ml
	$(RMLC) $(RMLLIB) -runtime Rmltop rmltop_machine_controller.rml 
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_machine_controller.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_directives.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_main.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop.ml

rmltop: lib
	$(OCAMLC) -o rmltop -I +threads $(RMLLIB) unix.cma str.cma \
		threads.cma \
		rmllib.cma \
		rmltop_lexer.cmo \
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
		rmltop_machine_controller.cmo \
		rmltop_directives.cmo \
		rmltop_main.cmo

wc:
	wc -l *.ml *.mli *.rml *.rmli

clean: 
	rm -f *.cm* *.rzi 

realclean: clean
	rm -f *~ *.annot *.?annot rmltop \
		rmltop_machine_controller.ml \
		rmltop_machine_body.ml \
		rmltop_reactive_machine.mli \
		rmltop_lexer.ml \


cleanall: realclean

FORCE: