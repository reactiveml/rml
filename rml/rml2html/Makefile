BIN = rml2html

DIRECTORIES = global parsing static typing reac lk lco caml main 

GLOBAL = asttypes.mli \
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

MAIN = 	main/compiler.cmo \
	main/errors.cmo \
	main/main.cmo \

SRC = $(GLOBAL) $(PARSING) $(MAIN)

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
	main/errors.cmo \
	main/compiler.cmo \
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
