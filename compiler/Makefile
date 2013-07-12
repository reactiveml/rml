include ../config

BIN:=$(COMPILER).$(TARGET)

.PHONY: all clean native byte clean debug install

all: $(TARGET)

native: byte
	$(OCAMLBUILD) $(COMPILER).native

byte:
	$(OCAMLBUILD) $(COMPILER).byte

debug:
	$(OCAMLBUILD) $(COMPILER).d.byte

profile:
	$(OCAMLBUILD) $(COMPILER).p.native

install: all
	$(INSTALL) -d $(INSTALL_BINDIR)
	$(INSTALL) $(COMPILER).$(TARGET) $(INSTALL_BINDIR)/$(COMPILER)

clean:
	$(OCAMLBUILD) -clean
