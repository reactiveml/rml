N=-1
SAMPLING=-1

all: simul simul.opt

rml:
	rmlc init.rml
	rmlc position.rml
	rmlc pos_tbl.rmli
	rmlc global.rml
	rmlc area.rml
	rmlc rml_graphics.rml
	rmlc draw.rml
	rmlc move.rml
	rmlc msg.rml
	rmlc routing.rml
	rmlc node.rml
	rmlc -s main -n $(N) -sampling $(SAMPLING) simul.rml

cmo: rml
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
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) routing.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) node.ml
	ocamlc -c -I `rmlc -where` $(OCAMLCFLAGS) simul.ml

cmx: rml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) init.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLCFLAGS) position.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) pos_tbl.mli
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) pos_tbl.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) global.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) area.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) rml_graphics.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) draw.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) move.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) msg.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) routing.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) node.ml
	ocamlopt -c -I `rmlc -where` $(OCAMLOPTFLAGS) simul.ml

simul: cmo
	ocamlc $(OCAMLCFLAGS) -o simul \
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
		routing.cmo \
		node.cmo \
		simul.cmo 

simul.opt: cmx
	ocamlopt $(OCAMLOPTFLAGS) -o simul.opt \
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
		routing.cmx \
		node.cmx \
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
	routing.ml \
	node.ml \
	simul.ml 

cleanall: clean
	rm -f simul simul.opt *~

realclean: cleanall