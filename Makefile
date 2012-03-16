.PHONY: all clean install

all:
	(cd compiler/; $(MAKE))
	(cd tools/; $(MAKE))
	(cd lib; $(MAKE))
	(cd mpi; $(MAKE))
	(cd interpreter; $(MAKE))

install:
	(cd compiler/; $(MAKE) install)
	(cd tools/; $(MAKE) install)
	(cd lib; $(MAKE) install)
	(cd mpi; $(MAKE) instal)
	(cd interpreter; $(MAKE) install)

clean:
	(cd compiler/; $(MAKE) clean)
	(cd tools/; $(MAKE) clean)
	(cd lib; $(MAKE) clean)
	(cd mpi; $(MAKE) clean)
	(cd interpreter; $(MAKE) clean)
