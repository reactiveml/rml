-include ../config

THEAD_TYPE=-thread

SRC=	sig_env.ml \
	lco_interpreter.mli \
	lco_ctrl_tree.ml \
	lco_ctrl_tree_thread_safe.ml \
	lco_ctrl_tree_n.ml \
	lco_rewrite.ml \
	lk_interpreter.mli \
	lk_implem.ml \
	lk_threaded.ml \
	implem.ml \
	rec_implem.ml \
	thread_implem.ml \
	rml_machine.ml \

OBJ=	sig_env.cmo \
	lco_ctrl_tree.cmo \
	lco_ctrl_tree_thread_safe.cmo \
	lco_ctrl_tree_n.cmo \
	lco_rewrite.cmo \
	lk_implem.cmo \
	lk_threaded.cmo \
	implem.cmo \
	rec_implem.cmo \
	thread_implem.cmo \
	rml_machine.cmo \



OBJ_OPT = $(OBJ:.cmo=.cmx)

INTF = $(OBJ:.cmo=.cmi) \
	lco_interpreter.cmi lk_interpreter.cmi

LIB= rmllib.cma rmllib.cmxa rmllib.a

INSTALLFILE= $(LIB) $(INTF)

all: lco_interpreter.cmi lk_interpreter.cmi $(LIB)

install: $(INSTALLFILE)
	$(INSTALL) -d $(LIBDIR)
	$(INSTALL) -m 644 $(INSTALLFILE) $(LIBDIR)

uninstall:
	(cd $(LIBDIR); rm -f $(INSTALLFILE) )


# cleaning:
clean:
	rm -f *.cm* *.annot *.o $(CIBLE) rmllib.a

realclean: clean
	rm -f  *~

cleanall: realclean

# utilities
depend:
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(INCLUDES) *.mli *.ml > .depend

wc:
	wc -l *.ml *.mli

# library
rmllib.cma: $(OBJ)
	$(OCAMLC) $(OCAMLFLAGS) -a -o rmllib.cma $^

rmllib.cmxa: $(OBJ_OPT)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o rmllib.cmxa $^


# dependencies
lco_rewrite.cmo: OCAMLFLAGS += -rectypes
lco_rewrite.cmx: OCAMLOPTFLAGS += -rectypes

rec_implem.cmo: OCAMLFLAGS += -rectypes
rec_implem.cmx: OCAMLOPTFLAGS += -rectypes

lco_ctrl_tree_thread_safe.cmo: OCAMLFLAGS += $(THEAD_TYPE)
lco_ctrl_tree_thread_safe.cmx: OCAMLOPTFLAGS += -thread

lk_threaded.cmo: OCAMLFLAGS += $(THEAD_TYPE)
lk_threaded.cmx: OCAMLOPTFLAGS += -thread

thread_implem.cmo: OCAMLFLAGS += $(THEAD_TYPE)
thread_implem.cmx: OCAMLOPTFLAGS += -thread

include .depend

.PHONY: all install uninstall clean realclean .depend wc
