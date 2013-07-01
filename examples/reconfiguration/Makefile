all:
	rmlc reconfiguration.rml
	ocamlc -c -I `rmlc -where` reconfiguration.ml

clean: 
	rm -f reconfiguration.ml reconfiguration.rzi *.cm*

realclean: clean
	rm -f *~
cleanall: realclean

