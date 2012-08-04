
NAME=toplevel
OBJS=dragnDrop.cmo $(NAME).cmo

all: $(NAME).js

DERIVING := $(shell ocamlfind query deriving-ocsigen 2> /dev/null)

JSLIB=js-of-ocaml
LIBDIR=js-of-ocaml/lib
RUNDIR=js-of-ocaml/runtime

JSFILES=\
 $(RUNDIR)/runtime.js \
 $(RUNDIR)/weak.js \
 scripts/toplevel_runtime.js \
 scripts/graphics.js

OCAMLC=ocamlfind ocamlc -annot \
 -package lwt,str \
 -I $(JSLIB)/compiler \
 -I $(LIBDIR) \
 -I $(LIBDIR)/syntax \
 -pp "camlp4o $(LIBDIR)/syntax/pa_js.cmo" \
 -I +threads \
 -I +graphics \
 -I ../ \
 -I ../../interpreter/ \
 -I ../../compiler/

OPTIONS+=\
 -I ../../interpreter/ \
 -I ../../compiler/ \
 -I ../../compiler/global \
 -I ../../compiler/lco \
 -I ../../compiler/typing \
 -I ../../compiler/other_analysis \
 -I ../../compiler/reac \
 -I ../../compiler/caml \
 -I ../../compiler/parsing \
 -I ../../compiler/optimization \
 -I ../../compiler/static \
 -I ../../compiler/main \
 -I ../../compiler/lk \
 -I ../../compiler/external \
 -I ../

STDLIB=\
 unix.cma \
 threads.cma \
 graphics.cma \
 js_of_ocaml.cma \
 compiler.cma \
 rmlcompiler.cmo \
 sig_env.cmo \
 lco_ctrl_tree.cmo \
 lco_ctrl_tree_n.cmo \
 lk_implem.cmo \
 implem.cmo \
 rmltop_lexer.cmo \
 toplevellib.cma \
 rmltop_global.cmo \
 rmltop_implem.cmo \
 rmltop_machine_body.cmo \
 rmltop_reactive_machine.cmo \
 rmltop_controller.cmo \
 rmltop_core.cmo \

EXPUNGE=$(shell ocamlc -where)/expunge
# Removed gc and sys
STDLIB_MODULES=\
  sys\
  graphics\
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
  rmlcompiler \
  rmltop_global \
  rmltop_core \
  rmltop_implem \
  rmltop_machine_body \
  rmltop_reactive_machine \
  rmltop_controller \
  weak
PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop
#STDLIB_MODULES=$(shell ocamlobjinfo toplevel.byte.tmp | sed -rn "s/^\t.*\t//p" | sort -u | tr '[:upper:]' '[:lower:]' | egrep -v "(sys|gc|bytegen|bytelibrarian|bytelink|bytepackager|bytesections|code|compile|conditioni|dgraph|dll|dom|driver|event|file|firebug|flow|form|freevars|generate|genprintval|inline|instr|javascript|js|lambda|linker|lwt|main_args|matching|meta|mutex|phisimpl|pparse|pretty_print|primitive2|pure_fun|regexp|runtimedef|simplif|switch|symtable|tailcall|thread|toplevel|trace|translclass|translcore|translmod|translobj|typed_array|typeopt|url|util|unix)")

js-of-ocaml-stamp:
	$(MAKE) -C js-of-ocaml all
	$(MAKE) -C js-of-ocaml/compiler compiler.cma
	touch $@

$(NAME).js: $(NAME).byte $(JSFILES)
	js-of-ocaml/compiler/js_of_ocaml -toplevel -noinline -noruntime $(JSFILES) $(NAME).byte $(OPTIONS)

.PHONY: toplevel.byte toplevel.js
$(NAME).byte: js-of-ocaml-stamp dragnDrop.cmi $(OBJS)
	$(OCAMLC) -linkall -linkpkg -o $@.tmp $(STDLIB) $(OBJS)
	$(EXPUNGE) $@.tmp $@ $(PERVASIVES)
	rm -f $@.tmp

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

clean::
	rm -f *.cm[io] $(NAME).byte $(NAME).js *.annot

cleanall: clean
	make -C js-of-ocaml clean
	rm -f js-of-ocaml-stamp

depend:
	ocamldep -pp "camlp4o $(LIBDIR)/syntax/pa_js.cmo" *.ml *.mli > .depend

tryrml-bundle.tar.gz:
	mkdir tryrml-bundle
	cp $(NAME).js index.html style.css tryrml-bundle/
	cp -r lessons tryrml-bundle/
	tar czvf try-rmlbundle.tar.gz tryrml-bundle/
	-rm -rf tryrml-bundle/

-include .depend
