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

distclean:clean
	rm -rf config.status config.log autom4te.cache/ config rmlbuild.config