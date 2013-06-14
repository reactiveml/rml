include ../../../config

.PHONY: clean FORCE

byte:
	$(RPMLBUILD) -use-ocamlfind -no-links rmlsdl.cma

native:
	$(RPMLBUILD) -use-ocamlfind -no-links rmlsdl.cmxa

clean:
	rm -rf _build

FORCE:
