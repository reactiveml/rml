include ../config
include ../configure-tools/Makefile.common

RMLC=../compiler/rmlc
RMLLIB=-I ../stdlib -I ../interpreter

INSTALLFILE=	rmltop.cmi \
		rmltop_reactive_machine.cmi \
		rmltop_reactive_machine.cmo \
		rmltop_machine_body.cmi \
		rmltop_machine_body.cmo \
		rmltop_controller.rzi \
		rmltop_controller.cmi \
		rmltop_controller.cmo \
		rmltop_global.rzi \
		rmltop_global.cmi \
		rmltop_global.cmo \
		rmltop_implem.cmi \
		rmltop_implem.cmo \
		rmltop_lexer.cmo \
		rmltop_main.cmi \
		rmltop.cmo \
		rmltop_main.cmo

GENSOURCES=	rmltop_controller.ml \
		rmltop_machine_body.ml \
		rmltop_reactive_machine.mli \
		rmltop_lexer.ml

all: rmltop lib

CL_INC=-I +ocamlbuild

rmltop: ../compiler/rmlcompiler.cmo \
	rmltop_lexer.cmo \
	rmltop_library.cmo \
	rmltop.cmo \
	rmlrun.cma
	$(OCAMLC) $(OCAMLFLAGS) -o rmltop $(CL_INC) -I +threads -I ../compiler $(RMLLIB) \
		toplevellib.cma \
		unix.cma \
		str.cma \
		threads.cma \
		ocamlbuildlib.cma \
		rmlcompiler.cmo \
		rmllib.cma \
		rmltop_lexer.cmo \
		rmltop_library.cmo \
		rmltop.cmo

rmlrun.cma: \
	rmltop_global.cmo \
	rmltop_implem.cmo \
	rmltop_machine_body.cmo \
	rmltop_reactive_machine.cmo \
	rmltop_controller.cmo \
	rmltop_main.cmo
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $^

lib: $(INSTALLFILE)

install: install.lib install.bin

install.bin: rmltop
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) rmltop $(BINDIR)/rmltop

install.lib: lib
	$(INSTALL) -d $(LIBDIR)/toplevel
	$(INSTALL) -m 644 $(INSTALLFILE) $(LIBDIR)/toplevel

test:
	rlwrap ./rmltop -n -I . -I ../interpreter

wc:
	wc -l *.*ml*

clean:
	rm -f *.cm* *.rzi

realclean: clean
	rm -f *~ *.annot *.?annot rmltop $(GENSOURCES)

cleanall: realclean

##

rmltop.cmi: rmltop_lexer.cmi rmltop_global.cmi

rmltop_lexer.ml: rmltop_lexer.mll
	$(OCAMLLEX) rmltop_lexer.mll

rmltop_lexer.cmo: rmltop_lexer.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_lexer.ml

rmltop_global.rzi: rmltop_global.rmli
	$(RMLC) -c $(RMLLIB) rmltop_global.rmli

rmltop_global.cmi: rmltop_global.mli
	$(OCAMLC) $(OCAMLFLAGS) $(RMLLIB) rmltop_global.mli

rmltop_global.cmo: rmltop_global.ml rmltop_global.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_global.ml

rmltop_machine_body.ml: rmltop_machine_body.rml
	$(RMLC) $(RMLLIB) -runtime Lco_ctrl_tree rmltop_machine_body.rml

rmltop_machine_body.cmi rmltop_machine_body.cmo: rmltop_machine_body.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_machine_body.ml

rmltop_reactive_machine.rzi rmltop_reactive_machine.mli: \
		rmltop_reactive_machine.rmli rmltop_global.rzi
	$(RMLC) $(RMLLIB) rmltop_reactive_machine.rmli

rmltop_reactive_machine.cmi: rmltop_reactive_machine.mli rmltop_global.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(RMLLIB) rmltop_reactive_machine.mli

rmltop_reactive_machine.cmo: rmltop_reactive_machine.ml \
		rmltop_machine_body.cmo rmltop_global.cmi \
		rmltop_reactive_machine.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_reactive_machine.ml

rmltop_controller.ml: rmltop_controller.rml rmltop_global.rzi
	$(RMLC) $(RMLLIB) -runtime Rmltop rmltop_controller.rml

rmltop_controller.cmi rmltop_controller.cmo: rmltop_controller.ml \
		rmltop_reactive_machine.cmi rmltop_implem.cmo rmltop_global.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_controller.ml

rmltop_implem.cmo: rmltop_implem.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_implem.ml

rmltop_main.cmi rmltop_main.cmo: rmltop_main.ml \
		rmltop_implem.cmo rmltop_global.cmi rmltop_controller.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_main.ml

rmltop.cmo: rmltop.ml ../compiler/rmlcompiler.cmo rmltop_lexer.cmo rmltop_global.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c $(CL_INC) -I ../compiler $(RMLLIB) -thread rmltop.ml

rmltop_library.cmo: errors.cmi ../compiler/rmlcompiler.cmo rmltop_lexer.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c $(CL_INC) -I ../compiler $(RMLLIB) -thread rmltop_library.ml

FORCE:
