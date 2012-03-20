include ../config

COBJS=init.o local_ref_impl.o local_token_impl.o mpi_impl.o
OBJS=mpi.cmo local_ref.cmo local_token.cmo

CFLAGS+=-I`$(OCAMLC) -where`
CC=$(MPICC)

.PHONY: all clean install
.PRECIOUS: %.cmi

all: libmlmpi.a mlmpi.cma mlmpi.cmxa

install: all

clean:
	rm -f *.cm* *.o *.a

libmlmpi.a: $(COBJS)
	rm -f $@
	ar rc $@ $(COBJS)

mlmpi.cma: $(OBJS)
	$(OCAMLC) -a -custom -o mlmpi.cma $(OBJS) -cc $(MPICC) -ccopt -L$(MLMPI_LIBDIR) -dllib -lmlmpi -cclib -lmlmpi

mlmpi.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o mlmpi.cmxa $(OBJS:.cmo=.cmx) -cc $(MPICC) -ccopt -L$(MLMPI_LIBDIR) -cclib -lmlmpi

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<
%.cmi: %.mli
	$(OCAMLC) -c $<
%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<
