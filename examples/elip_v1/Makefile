N=-1
SAMPLING=-1

SRC=	init.rml \
	position.rml \
	pos_tbl.rmli \
	global.rml \
	area.rml \
	rml_graphics.rml \
	draw.rml \
	move.rml \
	msg.rml \
	search_best_age.rmli \
	search_best_age.ml \
	routing.rml \
	node.rml \
	dynamic.rml \
	simul.rml

all: elip elip.opt

link:
	- ln -s pos_tbl_array_dynamic.rml pos_tbl.ml

rml: $(SRC)
	rmlc $(RMLCFLAGS) init.rml
	rmlc $(RMLCFLAGS) position.rml
	rmlc $(RMLCFLAGS) pos_tbl.rmli
	rmlc $(RMLCFLAGS) global.rml
	rmlc $(RMLCFLAGS) area.rml
	rmlc $(RMLCFLAGS) rml_graphics.rml
	rmlc $(RMLCFLAGS) draw.rml
	rmlc $(RMLCFLAGS) move.rml
	rmlc $(RMLCFLAGS) msg.rml
	rmlc $(RMLCFLAGS) search_best_age.rmli
	rmlc $(RMLCFLAGS) routing.rml
	rmlc $(RMLCFLAGS) node.rml
	rmlc $(RMLCFLAGS) dynamic.rml
	rmlc $(RMLCFLAGS) -s main -n $(N) -sampling $(SAMPLING) simul.rml

cmo: rml link
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) init.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) position.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) pos_tbl.mli
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) pos_tbl.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) global.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) area.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) rml_graphics.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) draw.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) move.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) msg.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) search_best_age.mli
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) search_best_age.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) routing.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) node.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) dynamic.ml
	ocamlfind ocamlc -c -I `rmlc -where` -package graphics $(OCAMLCFLAGS) simul.ml

cmx: rml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) init.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) position.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) pos_tbl.mli
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) pos_tbl.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) global.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) area.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) rml_graphics.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) draw.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) move.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) msg.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) search_best_age.mli
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) search_best_age.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) routing.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) node.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) dynamic.ml
	ocamlfind ocamlopt -c -I `rmlc -where` -package graphics $(OCAMLOPTFLAGS) simul.ml

elip: cmo
	ocamlfind ocamlc $(OCAMLCFLAGS) -o elip \
		-linkpkg -package graphics unix.cma \
		-I `rmlc -where` rmllib.cma \
		init.cmo \
		position.cmo \
		pos_tbl.cmo \
		global.cmo \
		area.cmo \
		rml_graphics.cmo \
		draw.cmo \
		move.cmo \
		msg.cmo \
		search_best_age.cmo \
		routing.cmo \
		node.cmo \
		dynamic.cmo \
		simul.cmo

elip.opt: cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -o elip.opt \
		-linkpkg -package graphics unix.cmxa \
		-I `rmlc -where` rmllib.cmxa \
		init.cmx \
		position.cmx \
		pos_tbl.cmx \
		global.cmx \
		area.cmx \
		rml_graphics.cmx \
		draw.cmx \
		move.cmx \
		msg.cmx \
		search_best_age.cmx \
		routing.cmx \
		node.cmx \
		dynamic.cmx \
		simul.cmx

clean:
	rm -f *.rzi *.annot *.?annot *.cmo *.cmx *.cmi *.o \
	init.ml \
	position.ml \
	pos_tbl.mli \
	global.ml \
	area.ml \
	rml_graphics.ml \
	draw.ml \
	move.ml \
	msg.ml \
	search_best_age.mli \
	routing.ml \
	node.ml \
	dynamic.ml \
	simul.ml

cleanall: clean
	rm -f elip elip.opt pos_tbl.ml *~

realclean: cleanall
