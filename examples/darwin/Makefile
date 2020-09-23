SAMPLING=0.005

darwin: darwin.rml
	rmlc -s main -sampling $(SAMPLING) darwin.rml
	ocamlfind ocamlc -o darwin -I `rmlc -where` -linkpkg -package graphics unix.cma rmllib.cma darwin.ml

clean: 
	rm -f darwin.rzi darwin.ml darwin.cm?

cleanall: clean
	rm -f darwin *~
realclean: cleanall
