include ../config

BIN:=$(COMPILER).$(TARGET)

.PHONY: all clean native byte clean debug install

all: $(TARGET)

native:
	$(OCAMLBUILD) $(COMPILER).native rpmlcompiler.cmx

byte:
	$(OCAMLBUILD) $(COMPILER).byte rpmlcompiler.cmo

debug:
	$(OCAMLBUILD) $(COMPILER).d.byte rpmlcompiler.d.cmo

profile:
	$(OCAMLBUILD) $(COMPILER).p.native rpmlcompiler.p.cmx

install: all
	$(INSTALL) -d $(INSTALL_BINDIR)
	$(INSTALL) $(COMPILER).$(TARGET) $(INSTALL_BINDIR)/$(COMPILER)

clean:
	$(OCAMLBUILD) -clean
