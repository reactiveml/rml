TEST=paper

all: $(TEST)

run: $(TEST)
	./$(TEST)

$(TEST): $(TEST).ml
	ocamlc -o $(TEST) -I `rmlc -runtime Lco_rewrite_proba -where` unix.cma graphics.cma rmllib.cma -rectypes $(TEST).ml

$(TEST).ml: $(TEST).rml
	rmlc -dtypes -runtime Lco_rewrite_proba $(TEST).rml



clean:
	rm -f $(TEST).ml $(TEST).cm* $(TEST).rzi $(TEST).sannot $(TEST).tannot
cleanall: clean
	rm -f $(TEST) *~
