OCAMLFIND=ocamlfind
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
RMLC=rmlc
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

byte: elip
opt: elip.opt

link:
	- ln -s pos_tbl_array_dynamic.rml pos_tbl.ml

rml: $(SRC)
	$(RMLC) $(RMLCFLAGS) init.rml
	$(RMLC) $(RMLCFLAGS) position.rml
	$(RMLC) $(RMLCFLAGS) pos_tbl.rmli
	$(RMLC) $(RMLCFLAGS) global.rml
	$(RMLC) $(RMLCFLAGS) area.rml
	$(RMLC) $(RMLCFLAGS) rml_graphics.rml
	$(RMLC) $(RMLCFLAGS) draw.rml
	$(RMLC) $(RMLCFLAGS) move.rml
	$(RMLC) $(RMLCFLAGS) msg.rml
	$(RMLC) $(RMLCFLAGS) search_best_age.rmli
	$(RMLC) $(RMLCFLAGS) stat.rml
	$(RMLC) $(RMLCFLAGS) routing.rml
	$(RMLC) $(RMLCFLAGS) node.rml
	$(RMLC) $(RMLCFLAGS) dynamic.rml
	$(RMLC) $(RMLCFLAGS) -n $(N) -sampling $(SAMPLING) simul.rml

cmo: rml link
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) init.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) position.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) pos_tbl.mli
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) pos_tbl.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) global.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) area.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) rml_graphics.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) draw.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) move.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) msg.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) search_best_age.mli
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) search_best_age.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) stat.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) routing.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) node.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) dynamic.ml
	$(OCAMLFIND) $(OCAMLC) -c -package graphics -I `$(RMLC) -where` $(OCAMLCFLAGS) simul.ml

cmx: rml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) init.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) position.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) pos_tbl.mli
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) pos_tbl.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) global.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) area.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) rml_graphics.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) draw.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) move.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) msg.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) search_best_age.mli
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) search_best_age.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) stat.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) routing.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) node.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) dynamic.ml
	$(OCAMLFIND) $(OCAMLOPT) -c -package graphics -I `$(RMLC) -where` $(OCAMLOPTFLAGS) simul.ml

elip: cmo
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -o elip \
		-linkpkg -package graphics unix.cma \
		-I `$(RMLC) -where` rmllib.cma \
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
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -o elip.opt \
		-linkpkg -package graphics unix.cmxa \
		-I `$(RMLC) -where` rmllib.cmxa \
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
	stat.ml \
	routing.ml \
	node.ml \
	dynamic.ml \
	simul.ml

cleanall: clean
	rm -f elip elip.opt pos_tbl.ml *~

realclean: cleanall
