BIN = rmldep

SRC= depend.ml rmldep.ml

DIRECTORIES = ../../compiler/global ../../compiler/parsing

INCLUDES=-I ../../compiler/global -I ../../compiler/parsing

OBJ =	../../compiler/global/version.cmo \
	../../compiler/global/rml_misc.cmo \
	../../compiler/global/warnings.cmo \
	../../compiler/global/rml_ident.cmo \
	../../compiler/parsing/linenum.cmo \
	../../compiler/parsing/parse_ident.cmo \
	../../compiler/parsing/parse_ast.cmo \
	../../compiler/parsing/location.cmo \
	../../compiler/parsing/rml_syntaxerr.cmo \
	../../compiler/parsing/rml_lexer.cmo \
	../../compiler/parsing/rml_parser.cmo \
	../../compiler/parsing/rml_parse.cmo \
	depend.cmo \
	rmldep.cmo

OBJ_OPT = $(OBJ:.cmo=.cmx)

INCLUDES = $(DIRECTORIES:%=-I %)

include ../../config
include ../../configure-tools/Makefile.common

all: $(TARGET)
install: install.$(TARGET)
	cp $(BINDIR)/$(BIN).$(TARGET) $(BINDIR)/$(BIN)

uninstall: uninstall.$(TARGET)
	rm -f $(BINDIR)/$(BIN)

# cleaning:
clean:
	rm -f *.cm? *.o

realclean: clean
	rm -f $(BIN).byte $(BIN).opt $(BIN) *~

cleanall: realclean

# utilities
wc:
	wc -l $(SRC)

include .depend
