include ../../config

.PHONY: clean all byte native

all: byte

byte:
	$(RPMLBUILD) -use-ocamlfind mlsdl_server.byte rmlsdl.cma

native:
	$(RPMLBUILD) -use-ocamlfind mlsdl_server.native rmlsdl.cmxa

clean:
	$(RPMLBUILD) -clean