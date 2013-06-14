include ../../config

.PHONY: clean FORCE

all: planets.rml.native planets_switch.rml.native planets_adapt.rml.native

rmlsdl_byte:
	(cd rmlsdl; $(MAKE) byte)

rmlsdl_native:
	(cd rmlsdl; $(MAKE) native)

%.rml.byte: rmlsdl_byte FORCE
	rm -f $@
	$(RPMLBUILD) -no-hygiene -use-ocamlfind -no-links $@
	cp _build/$@ .

%.rml.d.byte: rmlsdl_byte FORCE
	rm -f $@
	$(RPMLBUILD) -no-hygiene -use-ocamlfind -no-links $@
	cp _build/$@ .

%.rml.native: rmlsdl_native FORCE
	rm -f $@
	$(RPMLBUILD) -no-hygiene -use-ocamlfind -no-links $@
	cp _build/$@ .

clean:
	(cd rmlsdl; $(MAKE) clean)
	rm -rf _build
	rm -f *.byte *.native

FORCE:
