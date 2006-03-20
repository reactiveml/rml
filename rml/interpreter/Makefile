include ../config

SRC=	sig_env.ml \
	lco_interpreter.mli \
	lco_ctrl_tree.ml \
	lco_rewrite.ml \
	implantation.ml \
	lk_interpreter.ml \
	rml_machine.ml \

OBJ=	sig_env.cmo \
	lco_ctrl_tree.cmo \
	lco_rewrite.cmo \
	implantation.cmo \
	lk_interpreter.cmo \
	rml_machine.cmo \

#	lco_ctrl_tree_debug.cmo \

OBJ_OPT = $(OBJ:.cmo=.cmx)

INTF = $(OBJ:.cmo=.cmi) \
	lco_interpreter.cmi

LIB= rml_interpreter.cma rml_interpreter.cmxa rml_interpreter.a 

INSTALLFILE= $(LIB) $(INTF)

all: $(LIB)

install: $(INSTALLFILE)
	$(INSTALL) -d $(LIBDIR)
	$(INSTALL) -m 644 $(INSTALLFILE) $(LIBDIR)

uninstall:
	(cd $(LIBDIR); rm -f $(INSTALLFILE) )


# cleaning:
clean:
	rm -f *.cm* *.annot *.o $(CIBLE) rml_interpreter.a

realclean: clean
	rm -f  *~

# utilities
depend: 
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(INCLUDES) *.mli *.ml > .depend

wc:
	wc --lines *.ml *.mli

# library
rml_interpreter.cma: $(OBJ)
	$(OCAMLC) -a -o rml_interpreter.cma $^

rml_interpreter.cmxa: $(OBJ_OPT)
	$(OCAMLOPT) -a -o rml_interpreter.cmxa $^


# dependencies
lco_rewrite.cmo: OCAMLFLAGS := -rectypes
lco_rewrite.cmx: OCAMLOPTFLAGS := -rectypes

include .depend

.PHONY: all install uninstall clean realclean .depend wc 
