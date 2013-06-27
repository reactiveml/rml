include ../config

STDLIB_INTERFACE=random.rmli list.rmli rmltest.mli \
		 rmlarg.rmli graphics.rmli unix.rmli array.rmli \
		 lexing.rmli string.rmli sys.rmli gc.rmli obj.rmli
STDLIB_OBJ_TMP=$(STDLIB_INTERFACE:%.rmli=%.rzi)
STDLIB_OBJ=$(STDLIB_OBJ_TMP:%.mli=%.rzi)

PERVASIVES_INTERFACE=pervasives.rmli rml_pervasives.rmli
PERVASIVES_OBJ=$(PERVASIVES_INTERFACE:%.rmli=%.rzi)

.SUFFIXES: .rmli .mli .rzi

.rmli.rzi:
	../compiler/$(COMPILER).$(TARGET) -c $<
.mli.rzi:
	../compiler/$(COMPILER).$(TARGET) -c $<

.PHONY: all install clean pervasives

all: pervasives $(STDLIB_OBJ)
	(cd row; $(MAKE))

pervasives:
	../compiler/$(COMPILER).$(TARGET) -c -nopervasives pervasives.rmli
	../compiler/$(COMPILER).$(TARGET) -c -nopervasives rml_pervasives.rmli

install: all
	$(INSTALL) -d $(INSTALL_LIBDIR)
	$(INSTALL) $(PERVASIVES_OBJ) $(PERVASIVES_INTERFACE) $(INSTALL_LIBDIR)
	$(INSTALL) $(STDLIB_OBJ) $(STDLIB_INTERFACE) $(INSTALL_LIBDIR)
	(cd row; $(MAKE) install)

clean:
	rm -f *.rzi
	(cd row; $(MAKE) clean)

