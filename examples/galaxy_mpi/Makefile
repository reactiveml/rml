include ../../config

.PHONY: clean

%.rml.byte:
	(cd ../rmlsdl; $(MAKE) byte)
	$(RPMLBUILD) -use-ocamlfind $@

%.rml.native:
	(cd ../rmlsdl; $(MAKE) native)
	$(RPMLBUILD) -use-ocamlfind $@

clean:
	$(RPMLBUILD) -clean
