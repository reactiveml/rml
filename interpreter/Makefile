include ../config

RPMLLIB=rpmllib.cma rpmllib_mpi.cma


.PHONY: all clean native byte clean debug install

all: byte native

native:
	$(OCAMLBUILD) $(RPMLLIB:.cma=.cmxa)

byte:
	$(OCAMLBUILD) $(RPMLLIB)

install: all
	$(INSTALL) -d $(INSTALL_BINDIR)
	$(INSTALL) $(RPMLLIB).$(TARGET) $(INSTALL_LIBDIR)

clean:
	$(OCAMLBUILD) -clean
