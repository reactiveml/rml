include ../../config

RPMLDEP=rpmldep

.PHONY: all clean native byte clean debug install

all: $(TARGET)

native:
	$(OCAMLBUILD) $(RPMLDEP).native

byte:
	$(OCAMLBUILD) $(RPMLDEP).byte

debug:
	$(OCAMLBUILD) $(RPMLDEP).d.byte

profile:
	$(OCAMLBUILD) $(RPMLDEP).p.native

install: all
	$(INSTALL) -d $(INSTALL_BINDIR)
	$(INSTALL) $(RPMLDEP).$(TARGET) $(INSTALL_BINDIR)/$(RPMLDEP)

clean:
	$(OCAMLBUILD) -clean
