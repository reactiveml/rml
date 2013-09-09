include ../config
include ../configure-tools/Makefile.common

RMLC=../compiler/rmlc
RMLLIB=-I ../stdlib -I ../interpreter -I ../interpreter/lco

INSTALLFILE=	rmltop.cmi \
		rmltop_global.cmi \
		rmltop_machine_body.cmi \
		rmltop_reactive_machine.cmi \
		rmltop_controller.cmi \
		rmltop_lexer.cmo \
		rmltop.cmo \
		rmltop_global.cmo \
		rmltop_global.rzi \
		rmltop_machine_body.cmo \
		rmltop_reactive_machine.cmo \
		rmltop_controller.cmo \
		rmltop_controller.rzi \
		rmltop_directives.cmi \
		rmltop_implem.cmi \
		rmltop_main.cmi \
		rmltop_directives.cmo \
		rmltop_implem.cmo \
		rmltop_main.cmo

GENSOURCES=	rmltop_controller.ml \
		rmltop_machine_body.ml \
		rmltop_reactive_machine.mli \
		rmltop_lexer.ml

all: rmltop lib

rmltop: rmltop_lexer.cmo rmltop_global.cmo rmltop_directives.cmo rmltop.cmo
	$(OCAMLC) -o rmltop -I +threads $(RMLLIB) unix.cma str.cma \
		threads.cma \
		lco/rmllib.cma \
		rmltop_lexer.cmo \
		rmltop_global.cmo \
		rmltop_directives.cmo \
		rmltop.cmo

lib: $(INSTALLFILE)


install: install.lib install.bin

install.bin: rmltop
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) rmltop $(BINDIR)/rmltop

install.lib: lib
	$(INSTALL) -d $(LIBDIR)/lco/toplevel
	$(INSTALL) -m 644 $(INSTALLFILE) $(LIBDIR)/lco/toplevel

uninstall:
	rm -f $(BINDIR)/rmltop
	rm -rf $(LIBDIR)/toplevel

test:
	ledit ocaml -I +threads $(RMLLIB) unix.cma threads.cma \
		rmllib.cma \
		rmltop_global.cmo \
		rmltop_implem.cmo \
		rmltop_controller.cmo \
		rmltop_directives.cmo \
		rmltop_main.cmo

wc:
	wc -l *.ml *.mli *.rml *.rmli *.mll

clean:
	rm -f *.cm* *.rzi

realclean: clean
	rm -f *~ *.annot *.?annot rmltop $(GENSOURCES)

cleanall: realclean

##

rmltop.cmi: rmltop_lexer.cmi rmltop_global.cmi rmltop_directives.cmi

rmltop_lexer.ml: rmltop_lexer.mll
	$(OCAMLLEX) rmltop_lexer.mll

rmltop_lexer.cmo: rmltop_lexer.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_lexer.ml

rmltop_global.mli rmltop_global.rzi: rmltop_global.rmli
	$(RMLC) -c $(RMLLIB) rmltop_global.rmli

rmltop_global.cmi: rmltop_global.mli
	$(OCAMLC) $(RMLLIB) rmltop_global.mli

rmltop_global.cmo: rmltop_global.ml rmltop_global.cmi
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_global.ml

rmltop_machine_body.ml: rmltop_machine_body.rml
	$(RMLC) $(RMLLIB) -runtime Lco_ctrl_tree rmltop_machine_body.rml

rmltop_machine_body.cmi rmltop_machine_body.cmo: rmltop_machine_body.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_machine_body.ml

rmltop_reactive_machine.rzi rmltop_reactive_machine.mli: \
		rmltop_reactive_machine.rmli rmltop_global.rzi
	$(RMLC) $(RMLLIB) rmltop_reactive_machine.rmli

rmltop_reactive_machine.cmi: rmltop_reactive_machine.mli rmltop_global.cmi
	$(OCAMLC) $(RMLLIB) rmltop_reactive_machine.mli

rmltop_reactive_machine.cmo: rmltop_reactive_machine.ml \
		rmltop_machine_body.cmo rmltop_global.cmi \
		rmltop_reactive_machine.cmi
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_reactive_machine.ml

rmltop_machine_body.ml: rmltop_global.cmi

rmltop_controller.ml rmltop_controller.rzi: rmltop_controller.rml rmltop_global.rzi \
		rmltop_reactive_machine.rzi
	$(RMLC) $(RMLLIB) -runtime Rmltop rmltop_controller.rml

rmltop_controller.cmi rmltop_controller.cmo: rmltop_controller.ml \
		rmltop_reactive_machine.cmi rmltop_implem.cmo rmltop_global.cmi
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_controller.ml

rmltop_implem.cmo rmltop_implem.cmi: rmltop_implem.ml
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_implem.ml

rmltop_main.cmi rmltop_main.cmo: rmltop_main.ml \
		rmltop_implem.cmo rmltop_global.cmi rmltop_controller.cmo
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_main.ml

rmltop_directives.cmo rmltop_directives.cmi: rmltop_directives.ml rmltop_global.cmi
	$(OCAMLC) -c $(RMLLIB) -thread rmltop_directives.ml

rmltop.cmo: rmltop.ml rmltop_lexer.cmo rmltop_directives.cmo
	$(OCAMLC) -c $(RMLLIB) -thread rmltop.ml

FORCE:
