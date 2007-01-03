FILES=	../reac/annot.ml \
	../global/asttypes.mli \
	../main/errors.ml \
	../global/global.ml \
	../parsing/lexer.mll \
	../parsing/linenum.mll\
	../parsing/location.ml \
	../global/modules.ml \
	../parsing/parse_ast.ml \
	../parsing/parse.ml \
	../parsing/parse_printer.ml \
	../parsing/parser.mly \
	../reac/reac_ast.ml \
	../parsing/syntaxerr.ml \
	../global/warnings.ml

all:

diff: FORCE
	./diff.sh

patch: FORCE
	./patch.sh


public-distrib: FORCE
	./diff.sh
	(for file in $(FILES); \
	 do rm -f $$file; \
	 done)

clean:


realclean: clean
	rm -f *~

cleanall: realclean

FORCE: