
all: async1

async1: async1.ml
	ocamlc -I ../../interpreter -g -thread unix.cma threads.cma rmllib.cma $^ -o $@

.SUFFIXES: .ml .rml

# Lco_rewrite Lco_ctrl_tree Lco_ctrl_tree_n Lco_ctrl_tree_class Lk
.rml.ml:
	../../compiler/rmlc -s main -runtime Lco_ctrl_tree_n $^

replay: clean async1

test:
	./test.sh

clean:
	rm -f async1 async1.ml *.cm* *.rzi *.o

.PHONY: replay clean
