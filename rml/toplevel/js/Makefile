
NAME=toplevel
OBJS=toplevel.cmo

all: $(NAME).js

DERIVING := $(shell ocamlfind query deriving-ocsigen 2> /dev/null)

LIBDIR=$(shell ocamlc -where)/js_of_ocaml
JSFILES=$(LIBDIR)/runtime.js $(LIBDIR)/weak.js toplevel_runtime.js
OCAMLC=ocamlfind ocamlc -annot -package lwt,str,js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -I +js_of_ocaml -I +threads -I ../../interpreter/ -I ../../compiler/global/ -I ../ -I ~/debian/pkg-ocaml-maint/js-of-ocaml/compiler
OPTIONS+=-I ../../interpreter/ -I ../../compiler/global/
STDLIB= unix.cma threads.cma js_of_ocaml.cma ../../compiler/global/version.cmo compiler.cma ../../interpreter/sig_env.cmo ../../interpreter/lco_ctrl_tree.cmo ../../interpreter/implem.cmo ../rmltop_lexer.cmo ../../compiler/rmlcompiler.cmo toplevellib.cma ../rmltop_library.cmo ../../toplevel/rmlrun.cma
EXPUNGE=$(shell ocamlc -where)/expunge
# Removed gc and sys
STDLIB_MODULES=\
  arg \
  array \
  arrayLabels \
  buffer \
  callback \
  camlinternalLazy \
  camlinternalMod \
  camlinternalOO \
  char \
  complex \
  digest \
  filename \
  format \
  genlex \
  hashtbl \
  int32 \
  int64 \
  lazy \
  lexing \
  list \
  listLabels \
  map \
  marshal \
  moreLabels \
  nativeint \
  obj \
  oo \
  parsing \
  pervasives \
  printexc \
  printf \
  queue \
  random \
  scanf \
  set \
  sort \
  stack \
  stdLabels \
  stream \
  string \
  stringLabels \
  sig_env \
  lco_ctrl_tree \
  implem \
  weak
PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

#toplevel.byte: $(OBJS:cmx=cmo) toplevel.cmo
#	ocamlfind ocamlc -linkall -g -package str -linkpkg toplevellib.cma -o $@.tmp $^

$(NAME).js: $(NAME).byte $(JSFILES)
	js_of_ocaml -toplevel -noinline -noruntime $(JSFILES) $(NAME).byte $(OPTIONS)

.PHONY: toplevel.byte toplevel.js
$(NAME).byte: $(OBJS)
	$(OCAMLC) -linkall -package str -linkpkg -o $@.tmp $(STDLIB) $(OBJS)
	$(EXPUNGE) $@.tmp $@ $(PERVASIVES)
	rm -f $@.tmp

%.cmo: %.ml
	$(OCAMLC) -c $<

errors.cmi: errors.mli
	$(OCAMLC) -c $<

clean::
	rm -f *.cm[io] $(NAME).byte $(NAME).js

depend:
	ocamldep -pp "camlp4o $(LIBDIR)/pa_js.cmo" *.ml *.mli > .depend

-include .depend
