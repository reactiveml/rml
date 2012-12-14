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

arg.rzi: pervasives.rzi
array.rzi: pervasives.rzi
buffer.rzi: pervasives.rzi
callback.rzi: pervasives.rzi
char.rzi: pervasives.rzi
complex.rzi: pervasives.rzi
digest.rzi: pervasives.rzi
filename.rzi: pervasives.rzi
format.rzi: pervasives.rzi buffer.rzi
gc.rzi: pervasives.rzi
genlex.rzi: pervasives.rzi stream.rzi
graphics.rzi: pervasives.rzi
hashtbl.rzi: pervasives.rzi
int32.rzi: pervasives.rzi
int64.rzi: pervasives.rzi nativeint.rzi int32.rzi
lazy.rzi: pervasives.rzi
lexing.rzi: pervasives.rzi
list.rzi: pervasives.rzi
luc4ocaml_nolbl.rzi: pervasives.rzi luc4ocaml.rzi
luc4ocaml.rzi: pervasives.rzi
marshal.rzi: pervasives.rzi
nativeint.rzi: pervasives.rzi int32.rzi
obj.rzi: pervasives.rzi
parsing.rzi: pervasives.rzi obj.rzi lexing.rzi
printexc.rzi: pervasives.rzi
printf.rzi: pervasives.rzi buffer.rzi
queue.rzi: pervasives.rzi
random.rzi: pervasives.rzi
sort.rzi: pervasives.rzi
stack.rzi: pervasives.rzi
stream.rzi: pervasives.rzi
string.rzi: pervasives.rzi
str.rzi: pervasives.rzi
sys.rzi: pervasives.rzi
unix.rzi: pervasives.rzi
weak.rzi: pervasives.rzi

.PHONY:opt byte all install uninstall clean realclean
