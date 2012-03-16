include ../../config

.PHONY: clean byte all

all: byte

byte:
	(cd ../rmlsdl; $(MAKE) byte)
	$(RPMLBUILD) collision_nosig.rml.byte

clean:
	$(RPMLBUILD) -clean