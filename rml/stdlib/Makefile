# Makefile for Reactive-ML
# Taken from Lucid-synchrone

include ../config

RMLC := ../compiler/rmlc

#RMLC:=../compiler/rmlc.byte

CIBLE= rml_pervasives.rzi \
       arg.rzi \
       array.rzi \
       char.rzi \
       gc.rzi \
       graphics.rzi \
       hashtbl.rzi \
       list.rzi \
       obj.rzi \
       random.rzi \
       string.rzi \
       sys.rzi \
       unix.rzi

all:  $(CIBLE)

install:
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
