include ../../config

STDLIB_INTERFACE=pervasives.rmli random.rmli list.rmli rmltest.mli \
		 rmlarg.rmli graphics.rmli unix.rmli array.rmli \
		 lexing.rmli string.rmli sys.rmli rml_pervasives.rmli gc.rmli obj.rmli
STDLIB_OBJ_TMP=$(STDLIB_INTERFACE:%.rmli=%.rzi)
STDLIB_OBJ=$(STDLIB_OBJ_TMP:%.mli=%.rzi)

PERVASIVES_INTERFACE=pervasives.rmli rml_pervasives.rmli
PERVASIVES_OBJ=$(PERVASIVES_INTERFACE:%.rmli=%.rzi)

.SUFFIXES: .rmli .mli .rzi

.rmli.rzi:
	../../compiler/$(COMPILER).$(TARGET) -c -row-clocking $<
.mli.rzi:
	../../compiler/$(COMPILER).$(TARGET) -c -row-clocking $<

.PHONY: all install clean pervsaives

all: pervasives $(STDLIB_OBJ)

pervasives:
	../../compiler/$(COMPILER).$(TARGET) -c -nopervasives -row-clocking pervasives.rmli
	../../compiler/$(COMPILER).$(TARGET) -c -nopervasives -row-clocking rml_pervasives.rmli

install: all
	$(INSTALL) -d $(INSTALL_LIBDIR)/row
	$(INSTALL) $(STDLIB_OBJ) $(STDLIB_INTERFACE) $(INSTALL_LIBDIR)/row
	$(INSTALL) $(PERVASIVES_OBJ) $(PERVASIVES_INTERFACE) $(INSTALL_LIBDIR)/row

clean:
	rm -f *.rzi
