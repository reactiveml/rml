include ../config

ifeq ($(ENABLE_MPI), yes)
RPMLLIB=rpmllib.cma rpmllib_mpi.cma
else
RPMLLIB=rpmllib.cma
endif

ifeq ($(RELEASE_MODE), yes)
OCAMLBUILD_OPTIONS= -use-ocamlfind
else
OCAMLBUILD_OPTIONS= -use-ocamlfind -cflags -ppopt,-DRML_DEBUG
endif

HEADERS=lco_ctrl_tree_n.cmi rml_machine.cmi rml_types.cmi rmlarg.cmi\
	 rmltest.cmi runtime.cmi runtime_options.cmi seq_runtime.cmi sig_env.cmi

ifeq ($(ENABLE_MPI), yes)
HEADERS+=distributed_runtime.cmi distributed_runtime_old.cmi rmltest_mpi.cmi rml_machine_mpi.cmi
endif

.PHONY: all clean native byte clean debug install

all: byte native

native: byte
	$(OCAMLBUILD) $(OCAMLBUILD_OPTIONS) $(RPMLLIB:.cma=.cmxa)

byte:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTIONS) $(RPMLLIB)

install: all
	$(INSTALL) -d $(INSTALL_LIBDIR)
ifeq ($(TARGET), native)
	$(INSTALL) $(patsubst %,_build/%,$(RPMLLIB:.cma=.cmxa)) $(INSTALL_LIBDIR)
	$(INSTALL) $(patsubst %,_build/%,$(RPMLLIB:.cma=.a)) $(INSTALL_LIBDIR)
endif
	$(INSTALL) $(patsubst %,_build/%,$(RPMLLIB)) $(INSTALL_LIBDIR)
	$(INSTALL) $(patsubst %,_build/%,$(HEADERS)) $(INSTALL_LIBDIR)

clean:
	$(OCAMLBUILD) -clean
