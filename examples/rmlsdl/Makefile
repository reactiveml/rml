include ../../config

.PHONY: clean all byte

all: byte

byte:
	$(RPMLBUILD) mlsdl_server.byte rmlsdl.cma

clean:
	$(RPMLBUILD) -clean