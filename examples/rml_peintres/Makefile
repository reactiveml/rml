RPMLBUILD=rmlbuild
OCAMLBUILD=ocamlbuild
SDLOPTS:=-cclib,$(shell sdl-config --libs | sed -e "s/,/,-cclib,/g" | sed -e "s/ /,-cclib,/g")

.PHONY: clean FORCE

all: asynchrone.native asynchrone_yield.native asynchrone_delay.native asynchrone_mutex.native synchrone.rml.native

%.rml.native: FORCE
	rm -f $@
	$(RPMLBUILD) -use-ocamlfind -no-links -lflags $(SDLOPTS) $@
	cp _build/$@ .

%.native: FORCE
	rm -f $@
	$(OCAMLBUILD) -use-ocamlfind -no-links -lflags $(SDLOPTS) $@
	cp _build/$@ .

clean:
	rm -rf _build
	rm -f *.byte *.native

test: FORCE
	cd test && ./gen_tests > CTestTestfile.cmake
	cd test/ && ctest

FORCE:
