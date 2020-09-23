# Warning: This file has been modified to be ReactiveML compatible.

# Makefile for ReactiveML
# Taken from Lucid-synchrone

include ../config
include ../configure-tools/Makefile.common
include Makefile-stdlib.common


.SUFFIXES : .rmli .rzi
.rmli.rzi:
	$(RMLC) -c $<

all: stdlib.rzi $(STDLIB_SRC:.rmli=.rzi) $(STDLIB_THREAD_SRC:.rmli=.rzi)

install:

uninstall:

clean:
	rm -f *.rzi

realclean: clean
	rm -f *~
cleanall: realclean

arg.rzi: stdlib.rzi
array.rzi: stdlib.rzi
buffer.rzi: stdlib.rzi
bytes.rzi: stdlib.rzi
callback.rzi: stdlib.rzi
char.rzi: stdlib.rzi
complex.rzi: stdlib.rzi
digest.rzi: stdlib.rzi
filename.rzi: stdlib.rzi
format.rzi: stdlib.rzi buffer.rzi
gc.rzi: stdlib.rzi
genlex.rzi: stdlib.rzi stream.rzi
graphics.rzi: stdlib.rzi
hashtbl.rzi: stdlib.rzi
int32.rzi: stdlib.rzi
int64.rzi: stdlib.rzi nativeint.rzi int32.rzi
lazy.rzi: stdlib.rzi
lexing.rzi: stdlib.rzi
list.rzi: stdlib.rzi
luc4ocaml_nolbl.rzi: stdlib.rzi luc4ocaml.rzi
luc4ocaml.rzi: stdlib.rzi
marshal.rzi: stdlib.rzi
nativeint.rzi: stdlib.rzi int32.rzi
obj.rzi: stdlib.rzi
parsing.rzi: stdlib.rzi obj.rzi lexing.rzi
printexc.rzi: stdlib.rzi
printf.rzi: stdlib.rzi buffer.rzi
queue.rzi: stdlib.rzi
random.rzi: stdlib.rzi
sort.rzi: stdlib.rzi
stack.rzi: stdlib.rzi
stream.rzi: stdlib.rzi
string.rzi: stdlib.rzi
str.rzi: stdlib.rzi
sys.rzi: stdlib.rzi
unix.rzi: stdlib.rzi
weak.rzi: stdlib.rzi

rml_list.rzi: stdlib.rzi list.rzi
rml_process_manager.rzi: stdlib.rzi
thread/rml_async.rzi: stdlib.rzi thread/rml_async_body.rzi
thread/rml_async_body.rzi: stdlib.rzi

.PHONY:opt byte all install uninstall clean realclean
