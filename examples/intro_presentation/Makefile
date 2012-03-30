include ../../config

.PHONY: clean intro bfs all

all: intro bfs

intro:
	$(RPMLBUILD) intro.rml.byte

bfs:
	$(RPMLBUILD) bfs.rml.byte

clean:
	$(RPMLBUILD) -clean