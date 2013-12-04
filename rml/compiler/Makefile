BIN = rmlc

DIRECTORIES = global parsing external static typing other_analysis reac lco lk caml optimization main


GLOBAL = global/asttypes.ml \
	global/global.ml \
	global/global_ident.ml \
	global/ident.ml \
	global/initialization.ml \
	global/misc.ml \
	global/modules.ml \
	global/def_modules.ml \
	global/rzi.ml \
	global/symbol_table.ml \
	global/warnings.ml \

PARSING = parsing/lexer.mll \
	parsing/linenum.mll \
	parsing/location.ml \
	parsing/parse_ast.ml \
	parsing/parse_ident.ml \
	parsing/parse.ml \
	parsing/parse_printer.ml \
	parsing/parser.mly \
	parsing/syntaxerr.ml \

EXTERNAL = external/lucky.ml \
	external/lucky_errors.ml \
	external/external.ml \

STATIC = static/def_static.ml \
	static/static.ml \
	static/static_errors.ml \
	static/static_printer.ml \

REAC =	reac/annot.ml \
	reac/binding_errors.ml \
	reac/parse2reac.ml \
	reac/reac2reac.ml \
	reac/reac_ast.ml \
	reac/reac_misc.ml \

TYPING = typing/def_types.ml \
	typing/reactivity_effects.ml \
	typing/reactivity_check.cmo \
	typing/types.ml \
	typing/types_printer.ml \
	typing/typing.ml \
	typing/typing_errors.ml \

OTHER_ANALYSIS = other_analysis/instantaneous_loop.ml \
	other_analysis/wf_rec.ml \


LCO =	lco/lco_ast.ml \
	lco/lco_misc.ml \
	lco/reac2lco.ml \

LK  =	lk/lk_ast.ml \
	lk/lk_misc.ml \
	lk/reac2lk.ml \

CAML = 	caml/caml2caml.ml \
	caml/caml_ast.ml \
	caml/caml_misc.ml \
	caml/lco2caml.ml \
	caml/lk2caml.ml \
	caml/print_caml_src.ml \

OPTIMIZATION = optimization/reac_optimization.ml \

MAIN = 	main/compiler.ml \
	main/configure.ml \
	main/options.ml \
	main/errors.ml \
	main/interactive.ml \
	main/main.ml \

SRC = $(GLOBAL) $(PARSING) $(REAC) $(TYPING) $(OTHER_ANALYSIS) $(EXTERNAL) $(STATIC) $(LCO) $(LK) $(CAML) $(MAIN)


OBJ =	global/version.cmo \
	global/asttypes.cmo \
	global/warnings.cmo \
	parsing/linenum.cmo \
	parsing/location.cmo \
	global/misc.cmo \
	global/ident.cmo \
	global/global_ident.cmo \
	global/global.cmo \
	static/def_static.cmo \
	typing/def_types.cmo \
	global/rzi.cmo \
	global/def_modules.cmo \
	global/modules.cmo \
	global/symbol_table.cmo \
	parsing/lexer.cmo \
	parsing/syntaxerr.cmo \
	main/errors.cmo \
	main/configure.cmo \
	main/options.cmo \
	parsing/parse_ast.cmo \
	parsing/parse_ident.cmo \
	parsing/parser.cmo \
	parsing/parse.cmo \
	parsing/parse_printer.cmo \
	external/lucky_errors.cmo \
	external/lucky.cmo \
	external/external.cmo \
	reac/reac_ast.cmo \
	reac/binding_errors.cmo \
	typing/types_printer.cmo \
	typing/reactivity_effects.cmo \
	typing/types.cmo \
	reac/reac_misc.cmo \
	reac/reac2reac.cmo \
	typing/reactivity_check.cmo \
	reac/parse2reac.cmo \
	global/initialization.cmo \
	static/static_printer.cmo \
	static/static_errors.cmo \
	static/static.cmo \
	other_analysis/wf_rec.cmo \
	other_analysis/instantaneous_loop.cmo \
	reac/annot.cmo \
	typing/typing_errors.cmo \
	typing/typing.cmo \
	lco/lco_ast.cmo \
	lco/lco_misc.cmo \
	lco/reac2lco.cmo \
	lk/lk_ast.cmo \
	lk/lk_misc.cmo \
	lk/reac2lk.cmo \
	caml/caml_ast.cmo \
	caml/caml_misc.cmo \
	caml/lco2caml.cmo \
	caml/lk2caml.cmo \
	caml/caml2caml.cmo \
	caml/print_caml_src.cmo \
	optimization/reac_optimization.cmo \
	main/compiler.cmo \
	main/interactive.cmo \
	main/main.cmo \


OBJ_OPT = $(OBJ:.cmo=.cmx)

INCLUDES = $(DIRECTORIES:%=-I %)

GENSOURCES = global/version.ml parsing/parser.mli parsing/lexer.ml parsing/parser.ml \
	parsing/linenum.ml

include ../config
include ../configure-tools/Makefile.common

OCAMLFLAGS+=-for-pack Rmlcompiler
OCAMLOPTFLAGS+=-for-pack Rmlcompiler

all:: $(TARGET) rmlcompiler.cmo
ifeq ($(TARGET), opt)
all:: rmlcompiler.cmx
endif

rzi:
	$(MAKE) $(TARGET)
	$(MAKE) -C ../stdlib all
	$(MAKE) -C ../toplevel-alt rmltop_alt_global.rzi
	-$(RM) -f global/rzi.ml
	$(MAKE) $(TARGET) rmlcompiler.cmo

install: install.$(TARGET)
	cp $(BINDIR)/$(BIN).$(TARGET) $(BINDIR)/$(BIN)
uninstall: uninstall.$(TARGET)
	rm -f $(BINDIR)/$(BIN)


C_OBJ_BYTE = $(filter-out main/options.cmo main/main.cmo,$(OBJ))
C_OBJ_OPT  = $(C_OBJ_BYTE:.cmo=.cmx)

rmlcompiler.cmo: $(C_OBJ_BYTE)
	$(OCAMLC) -g -pack -o $@ $(INCLUDES) $^

rmlcompiler.cmx: $(C_OBJ_OPT)
	$(OCAMLOPT) -g -pack -o $@ $(INCLUDES) $^

diff: FORCE
	(cd diff; $(MAKE) diff)

# cleaning:
clean:
	(for d in . $(DIRECTORIES); \
	 do rm -f $$d/*.cmi $$d/*.cmo $$d/*.cmx $$d/*.o $$d/*.annot; \
	 done)

realclean: clean
	(for d in $(DIRECTORIES); \
	 do rm -f $$d/*~; \
	 done)
	rm -f $(BIN).byte $(BIN).opt $(GENSOURCES) parsing/parser.output
	rm -f rmlc global/version.ml global/rzi.ml *~

cleanall: realclean


# utilities
depend: $(GENSOURCES)
	(for d in $(DIRECTORIES); \
	 do $(OCAMLDEP) $(OCAMLDEPFLAGS) $(INCLUDES) $$d/*.mli $$d/*.ml; \
	 done) > .depend

wc:
	wc -l $(SRC)

# dependencies
parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(OCAMLYACC) -v parsing/parser.mly
lexer.cmx: parser.cmi

#.PHONY: global/rzi.ml
global/rzi.ml: global/rzi.cmi global/def_modules.cmo
	$(OCAML) -I global global/def_modules.cmo ../configure-tools/embedrzi.ml "../"

# version
global/version.ml:
	echo "(* This file is generated *)" > global/version.ml
	echo "let version = \"$(VERSION)\"" >> global/version.ml
	echo "let stdlib = \"$(LIBDIR)\"" >> global/version.ml

FORCE:

include .depend
