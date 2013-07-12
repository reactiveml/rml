include config

ifeq ($(ENABLE_MPI), yes)
SUBDIRS = compiler tools mpi lib interpreter
else
SUBDIRS = compiler tools lib interpreter
endif

.PHONY: all clean install

all:
	for d in $(SUBDIRS); do (cd $$d; $(MAKE)); done

install:
	for d in $(SUBDIRS); do (cd $$d; $(MAKE) install); done

clean:
	for d in $(SUBDIRS); do (cd $$d; $(MAKE) clean); done
	(cd toplevel-alt && $(MAKE) clean)
	(cd toplevel-alt/js && $(MAKE) clean)

distclean:clean
	rm -rf config.status config.log autom4te.cache/ config rmlbuild.config

toplevel:all
	(cd compiler && ocamlbuild rpmlcompiler.cmo)
	(cd toplevel-alt && $(MAKE) rmltop_alt_global.rzi)
	-rm compiler/global/rzi.ml
	$(OCAML) -I compiler/_build/global compiler/_build/global/def_modules.cmo toplevel-alt/embedrzi.ml .
	(cd compiler && ocamlbuild rpmlcompiler.cmo)
	(cd compiler && $(MAKE))
	(cd toplevel-alt && $(MAKE))
	(cd toplevel-alt/js && $(MAKE))