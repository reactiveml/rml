SAMPLING=0.005

darwin: darwin.rml
	rmlc -s main -sampling $(SAMPLING) darwin.rml
	ocamlc -o darwin -I `rmlc -where` unix.cma rmllib.cma graphics.cma  darwin.ml

clean: 
	rm -f darwin.rzi darwin.ml darwin.cm?

cleanall: clean
	rm -f darwin *~
realclean: cleanall
