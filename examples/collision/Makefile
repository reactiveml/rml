include ../../config

.PHONY: clean byte native all

all: byte

byte:
	(cd ../rmlsdl; $(MAKE) byte)
	$(RPMLBUILD) -use-ocamlfind collision_nosig.rml.byte

native:
	(cd ../rmlsdl; $(MAKE) native)
	$(RPMLBUILD) -use-ocamlfind collision_nosig.rml.native

clean:
	$(RPMLBUILD) -clean