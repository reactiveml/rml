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
	stat.rml \
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
	rmlc $(RMLCFLAGS) stat.rml
	rmlc $(RMLCFLAGS) routing.rml
	rmlc $(RMLCFLAGS) node.rml
	rmlc $(RMLCFLAGS) dynamic.rml
	rmlc $(RMLCFLAGS) -s main -n $(N) -sampling $(SAMPLING) simul.rml

cmo: rml link
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) init.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) position.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) pos_tbl.mli
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) pos_tbl.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) global.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) area.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) rml_graphics.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) draw.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) move.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) msg.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) search_best_age.mli
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) search_best_age.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) stat.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) routing.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) node.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) dynamic.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) simul.ml

cmx: rml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) init.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) position.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) pos_tbl.mli
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) pos_tbl.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) global.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) area.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) rml_graphics.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) draw.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) move.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) msg.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) search_best_age.mli
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) search_best_age.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) stat.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) routing.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) node.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) dynamic.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) simul.ml

elip: cmo
	ocamlc $(OCAMLCFLAGS) -o elip \
		unix.cma graphics.cma \
		-I `rmlc -where` rml_interpreter.cma \
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
		stat.cmo \
		routing.cmo \
		node.cmo \
		dynamic.cmo \
		simul.cmo 

elip.opt: cmx
	ocamlopt $(OCAMLOPTFLAGS) -o elip.opt \
		unix.cmxa graphics.cmxa \
		-I `rmlc -where` rml_interpreter.cmxa \
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
		stat.cmx \
		routing.cmx \
		node.cmx \
		dynamic.cmx \
		simul.cmx 

clean:
	rm -f *.rzi *.annot *.rannot *.cmo *.cmx *.cmi *.o \
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
	stat.ml \
	routing.ml \
	node.ml \
	dynamic.ml \
	simul.ml 

cleanall: clean
	rm -f elip elip.opt pos_tbl.ml *~

realclean: cleanall
