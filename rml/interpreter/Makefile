include ../config

OBJ=	lk_rewrite.cmo \
	lk_rewrite_debug.cmo \
	lco_ctrl_tree.cmo \

#	lco_ctrl_tree_debug.cmo \

OBJ_OPT = $(OBJ:.cmo=.cmx)

INTF = $(OBJ:.cmo=.cmi) \
	lk_interpreter.cmi \
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
	wc --lines $$d/*.ml $$d/*.mli

# library
rml_interpreter.cma: $(OBJ)
	$(OCAMLC) -a -o rml_interpreter.cma $^

rml_interpreter.cmxa: $(OBJ_OPT)
	$(OCAMLOPT) -a -o rml_interpreter.cmxa $^


# dependencies
lk_rewrite.cmo: OCAMLFLAGS := -rectypes
lk_rewrite.cmx: OCAMLOPTFLAGS := -rectypes

lk_rewrite_debug.cmo: OCAMLFLAGS := -rectypes
lk_rewrite_debug.cmx: OCAMLOPTFLAGS := -rectypes

include .depend

.PHONY: all install uninstall clean realclean depend wc 
