include ../config

RPMLLIB=rpmllib.cma rpmllib_mpi.cma

ifeq ($(RELEASE_MODE), yes)
OCAMLBUILD_OPTIONS= -use-ocamlfind
else
OCAMLBUILD_OPTIONS= -use-ocamlfind -cflags -ppopt,-DRML_DEBUG
endif

.PHONY: all clean native byte clean debug install

all: byte native

native:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTIONS) $(RPMLLIB:.cma=.cmxa)

byte:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTIONS) $(RPMLLIB)

install: all
	$(INSTALL) -d $(INSTALL_BINDIR)
	$(INSTALL) $(RPMLLIB).$(TARGET) $(INSTALL_LIBDIR)

clean:
	$(OCAMLBUILD) -clean
