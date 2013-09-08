include ../config
include ../configure-tools/Makefile.common

RMLC=../compiler/rmlc
RMLLIB=-I ../stdlib -I ../interpreter -I ../interpreter/lco_toplevel

INSTALLFILE=	rmltop_alt.cmi \
		rmltop_alt_reactive_machine.cmi \
		rmltop_alt_reactive_machine.cmo \
		rmltop_alt_controller.rzi \
		rmltop_alt_controller.cmi \
		rmltop_alt_controller.cmo \
		rmltop_alt_global.rzi \
		rmltop_alt_global.cmi \
		rmltop_alt_global.cmo \
		rmltop_alt_implem.cmi \
		rmltop_alt_implem.cmo \
		rmltop_alt_lexer.cmo \
		rmltop_alt.cmo

RMLRUNFILES = rmltop_alt_implem.cmo \
	rmltop_alt_reactive_machine.cmo \
	rmltop_alt_controller.cmo

GENSOURCES=	rmltop_alt_controller.ml \
		rmltop_alt_reactive_machine.mli \
		rmltop_alt_lexer.ml

all: rmltop_alt lib

CL_INC=-I +ocamlbuild -I +compiler-libs

rmltop_alt: ../compiler/rmlcompiler.cmo \
	$(RMLRUNFILES) \
	rmltop_alt_global.cmo \
	rmltop_alt_lexer.cmo \
	rmltop_alt_core.cmo \
	rmltop_alt.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o rmltop_alt $(CL_INC) -I +threads -I ../compiler $(RMLLIB) \
		ocamlcommon.cma \
		ocamlbytecomp.cma \
		ocamltoplevel.cma \
		unix.cma \
		str.cma \
		threads.cma \
		ocamlbuildlib.cma \
		rmlcompiler.cmo \
		rmltop_alt_global.cmo \
		rmltop_alt_lexer.cmo \
		rmllib.cma \
		rmltop_alt_implem.cmo \
		rmltop_alt_reactive_machine.cmo \
		rmltop_alt_controller.cmo \
		rmltop_alt_core.cmo \
		rmltop_alt.cmo

../compiler/rmlcompiler.cmo:
	$(MAKE) -C ../compiler rzi

lib: $(INSTALLFILE)

install: install.lib install.bin

install.bin: rmltop_alt
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) rmltop_alt $(BINDIR)/rmltop_alt

install.lib: lib
	$(INSTALL) -d $(LIBDIR)/toplevel
	$(INSTALL) -m 644 $(INSTALLFILE) $(LIBDIR)/toplevel

uninstall:
	rm -f $(BINDIR)/rmltop_alt
	rm -rf $(LIBDIR)/toplevel

test:
	./rmltop_alt -debug -n -I . -I ../interpreter

wc:
	wc -l *.ml *.mli *.rml *.rmli *.mll

clean:
	rm -f *.cm* *.rzi

realclean: clean
	rm -f *~ *.annot *.?annot rmltop_alt $(GENSOURCES)

cleanall: realclean

##

rmltop_alt.cmi: rmltop_alt_lexer.cmi rmltop_alt_global.cmi

rmltop_alt_lexer.ml: rmltop_alt_lexer.mll
	$(OCAMLLEX) rmltop_alt_lexer.mll

rmltop_alt_lexer.cmo: rmltop_alt_lexer.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_alt_lexer.ml

rmltop_alt_global.rzi: rmltop_alt_global.rmli
	$(RMLC) -c $(RMLLIB) rmltop_alt_global.rmli

rmltop_alt_global.cmi: rmltop_alt_global.mli
	$(OCAMLC) $(OCAMLFLAGS) $(RMLLIB) rmltop_alt_global.mli

rmltop_alt_global.cmo: rmltop_alt_global.ml rmltop_alt_global.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_alt_global.ml


rmltop_alt_reactive_machine.rzi rmltop_alt_reactive_machine.mli: \
		rmltop_alt_reactive_machine.rmli rmltop_alt_global.rzi
	$(RMLC) $(RMLLIB) rmltop_alt_reactive_machine.rmli

rmltop_alt_reactive_machine.cmi: rmltop_alt_reactive_machine.mli rmltop_alt_global.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(RMLLIB) rmltop_alt_reactive_machine.mli

rmltop_alt_reactive_machine.cmo: rmltop_alt_reactive_machine.ml \
		rmltop_alt_global.cmi \
		rmltop_alt_reactive_machine.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_alt_reactive_machine.ml

rmltop_alt_controller.ml: rmltop_alt_controller.rml rmltop_alt_global.rzi
	$(RMLC) $(RMLLIB) -runtime Rmltop_alt rmltop_alt_controller.rml

rmltop_alt_controller.cmi rmltop_alt_controller.cmo: rmltop_alt_controller.ml \
		rmltop_alt_reactive_machine.cmi rmltop_alt_implem.cmo rmltop_alt_global.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_alt_controller.ml

rmltop_alt_implem.cmo rmltop_alt_implem.cmi: rmltop_alt_implem.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(RMLLIB) -thread rmltop_alt_implem.ml

rmltop_alt.cmo: rmltop_alt.ml ../compiler/rmlcompiler.cmo rmltop_alt_lexer.cmo rmltop_alt_global.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c $(CL_INC) -I ../compiler $(RMLLIB) -thread rmltop_alt.ml

rmltop_alt_core.cmo: rmltop_alt_core.cmi errors.cmi ../compiler/rmlcompiler.cmo rmltop_alt_lexer.cmo rmltop_alt_global.cmo \
		rmltop_alt_implem.cmo rmltop_alt_global.cmi rmltop_alt_controller.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c $(CL_INC) -I ../compiler $(RMLLIB) -thread rmltop_alt_core.ml

FORCE:
