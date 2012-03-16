include ../config

RPMLLIB=rpmllib

.PHONY: all clean native byte clean debug install

all: $(TARGET)

native:
	$(OCAMLBUILD) $(RPMLLIB).cmxa

byte:
	$(OCAMLBUILD) $(RPMLLIB).cma

install: all
	$(INSTALL) -d $(INSTALL_BINDIR)
	$(INSTALL) $(RPMLLIB).$(TARGET) $(INSTALL_LIBDIR)

clean:
	$(OCAMLBUILD) -clean
