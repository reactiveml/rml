# Warning: This file has been modified to be ReactiveML compatible.

# Makefile for ReactiveML
# Taken from Lucid-synchrone

include ../config
include ../configure-tools/Makefile.common

RMLC := ../compiler/rmlc

#RMLC:=../compiler/rmlc.byte

SRC= pervasives.rmli \
	arg.rmli \
	array.rmli \
	buffer.rmli \
	callback.rmli \
	char.rmli \
	complex.rmli \
	digest.rmli \
	filename.rmli \
	format.rmli \
	gc.rmli \
	stream.rmli \
	genlex.rmli \
	graphics.rmli \
	hashtbl.rmli \
	int32.rmli \
	nativeint.rmli \
	int64.rmli \
	lazy.rmli \
	lexing.rmli \
	list.rmli \
	luc4ocaml.rmli \
	luc4ocaml_nolbl.rmli \
	marshal.rmli \
	obj.rmli \
	parsing.rmli \
	printexc.rmli \
	printf.rmli \
	queue.rmli \
	random.rmli \
	sort.rmli \
	stack.rmli \
	str.rmli \
	string.rmli \
	sys.rmli \
	unix.rmli \
	weak.rmli \
	rmltop_global.rmli \

CIBLE= $(SRC:.rmli=.rzi)

all:  $(CIBLE)

install: $(CIBLE)
	$(INSTALL) -d $(LIBDIR)
	$(INSTALL) -m 644 $(patsubst %.rzi,%.rmli,$(CIBLE)) $(LIBDIR)
	$(INSTALL) -m 644 $(CIBLE) $(LIBDIR)

uninstall:
	(cd $(LIBDIR); rm -f $(patsubst,*.rzi,*.rmli,$(CIBLE)) $(CIBLE) )

clean:
	rm -f *.rzi

realclean: clean
	rm -f *~

.PHONY:opt byte all install uninstall clean realclean
