BIN = rmlc

DIRECTORIES = global parsing external static typing reac lco lk caml main 


GLOBAL = global/asttypes.mli \
	global/global.ml \
	global/global_ident.ml \
	global/ident.ml \
	global/initialization.ml \
	global/misc.ml \
	global/misc_pp.mlp \
	global/modules.ml \
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

STATIC = static/def_static.mli \
	static/static.ml \
	static/static_errors.ml \

REAC =	reac/binding_errors.ml \
	reac/parse2reac.ml \
	reac/reac2reac.ml \
	reac/reac_ast.ml \
	reac/reac_misc.ml \

TYPING = typing/def_types.ml \
	typing/stypes.ml \
	typing/types.ml \
	typing/types_printer.ml \
	typing/typing.ml \
	typing/typing_errors.ml \


LCO =	lco/lco_ast.ml \
	lco/lco_misc.ml \
	lco/reac2lco.ml \

LK  =	lk/lk_ast.ml \
	lk/reac2lk.ml \

CAML = 	caml/caml_ast.ml \
	caml/caml_misc.ml \
	caml/lco2caml.ml \
	caml/lk2caml.ml \
	caml/print_caml_src.ml \

MAIN = 	main/compiler.ml \
	main/errors.ml \
	main/interactive.ml \
	main/main.ml \

SRC = $(GLOBAL) $(PARSING) $(REAC) $(TYPING) $(EXTERNAL) $(STATIC) $(LCO) $(LK) $(CAML) $(MAIN)


OBJ =   global/warnings.cmo \
	parsing/linenum.cmo \
	parsing/location.cmo \
	global/misc.cmo \
	global/ident.cmo \
	typing/def_types.cmo \
	global/modules.cmo \
	global/global_ident.cmo \
	global/global.cmo \
	global/symbol_table.cmo \
	parsing/lexer.cmo \
	parsing/syntaxerr.cmo \
	parsing/parse_ast.cmo \
	parsing/parse_ident.cmo \
	parsing/parser.cmo \
	parsing/parse.cmo \
	parsing/parse_printer.cmo \
	external/lucky_errors.cmo \
	external/lucky.cmo \
	external/external.cmo \
	static/static_errors.cmo \
	static/static.cmo \
	reac/reac_ast.cmo \
	reac/binding_errors.cmo \
	typing/types.cmo \
	reac/reac_misc.cmo \
	reac/parse2reac.cmo \
	global/initialization.cmo \
	reac/reac2reac.cmo \
	typing/types_printer.cmo \
	typing/stypes.cmo \
	typing/typing_errors.cmo \
	typing/typing.cmo \
	lco/lco_ast.cmo \
	lco/lco_misc.cmo \
	lco/reac2lco.cmo \
	lk/lk_ast.cmo \
	lk/reac2lk.cmo \
	caml/caml_ast.cmo \
	caml/caml_misc.cmo \
	caml/lco2caml.cmo \
	caml/lk2caml.cmo \
	caml/print_caml_src.cmo \
	main/errors.cmo \
	main/compiler.cmo \
	main/interactive.cmo \
	main/main.cmo \


OBJ_OPT = $(OBJ:.cmo=.cmx)

INCLUDES = $(DIRECTORIES:%=-I %)

GENSOURCES = parsing/parser.mli parsing/lexer.ml parsing/parser.ml \
	parsing/linenum.ml

include ../config

all: $(TARGET)
install: install.$(TARGET)

# cleaning:
clean:
	(for d in $(DIRECTORIES); \
         do rm -f $$d/*.cmi $$d/*.cmo $$d/*.cmx $$d/*.o $$d/*.annot; \
	 done)

realclean: clean
	rm -f $(BIN).byte $(BIN).opt $(GENSOURCES) parsing/parser.output
	rm -f rmlc stdlib

# utilities
depend: $(GENSOURCES)
	(for d in $(DIRECTORIES); \
	 do $(OCAMLDEP) $(OCAMLDEPFLAGS) $(INCLUDES) $$d/*.mli $$d/*.ml; \
	 done) > .depend

wc:
	(for d in $(DIRECTORIES); \
	 do wc --lines $$d/*.ml $$d/*.mli; \
	 done)

# dependencies
parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(OCAMLYACC) -v parsing/parser.mly
lexer.cmx: parser.cmi

global/misc.cmo: parsing/location.cmi global/misc_pp.cmo stdlib
global/misc.cmo: OCAMLC := $(TRUE_OCAMLC)
global/misc.cmo: OCAMLFLAGS := \
		-pp "camlp4o ./global/misc_pp.cmo" 
global/misc.cmx: parsing/location.cmi global/misc_pp.cmo stdlib
global/misc.cmx: OCAMLOPTFLAGS := \
		-pp "camlp4o ./global/misc_pp.cmo" 

global/misc_pp.cmo: global/misc_pp.mlp
	$(OCAMLC) -pp "camlp4o pa_extend.cmo q_MLast.cmo -impl" -I +camlp4 -c -impl global/misc_pp.mlp

stdlib: global/misc.ml
	echo $(LIBDIR) > stdlib

include .depend
