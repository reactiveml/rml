include ../config

STDLIB_INTERFACE=pervasives.rmli random.rmli list.rmli rmltest.mli rmlarg.rmli graphics.rmli unix.rmli
STDLIB_OBJ=$(STDLIB_INTERFACE:.rmli=.rzi)

.SUFFIXES: .rmli .mli .rzi

.rmli.rzi:
	../compiler/$(COMPILER).$(TARGET) -c $<
.mli.rzi:
	../compiler/$(COMPILER).$(TARGET) -c $<

.PHONY: all install clean

all: $(STDLIB_OBJ)

install: all
	$(INSTALL) -d $(INSTALL_LIBDIR)
	$(INSTALL) $(STDLIB_OBJ) $(STDLIB_INTERFACE) $(INSTALL_LIBDIR)

clean:
	rm -f *.rzi