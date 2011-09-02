# Makefile for Reactive-ML
# Taken from Lucid-synchrone

.SUFFIXES : .mli .ml .cmi .cmo .cmx .mll .rmli .rzi

.rmli.rzi:
	$(RMLC) -c $<

.mli.rzi:
	$(RMLC) -c $<

RMLC := ../compiler/rpmlc.byte

SRC= pervasives.rmli \
	list.rmli \
	random.rmli \

ML_SRC=rmltest.mli

CIBLE= $(SRC:.rmli=.rzi) $(ML_SRC:.mli=.rzi)

all:  $(CIBLE)

clean:
	rm -f *.rzi

.PHONY: all clean
