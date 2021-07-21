RMLC=rmlc
RMLBUILD=rmlbuild -classic-display
FLAGS=-tag annot

all: aquatic.cma


aquatic.cma:
	$(RMLBUILD) $(FLAGS) $@


clean:
	rm -rf _build
realclean: clean
	rm -f *~
cleanall: realclean
