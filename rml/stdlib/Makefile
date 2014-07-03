# Warning: This file has been modified to be ReactiveML compatible.

# Makefile for ReactiveML
# Taken from Lucid-synchrone

include ../config
include ../configure-tools/Makefile.common
include Makefile-stdlib.common


.SUFFIXES : .rmli .rzi
.rmli.rzi:
	$(RMLC) -c $<

all: pervasives.rzi $(STDLIB_SRC:.rmli=.rzi) $(STDLIB_THREAD_SRC:.rmli=.rzi)

install:

uninstall:

clean:
	rm -f *.rzi

realclean: clean
	rm -f *~
cleanall: realclean

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

rml_list.rzi: pervasives.rzi list.rzi
rml_process_manager.rzi: pervasives.rzi
thread/rml_async.rzi: pervasives.rzi thread/rml_async_body.rzi
thread/rml_async_body.rzi: pervasives.rzi

.PHONY:opt byte all install uninstall clean realclean
