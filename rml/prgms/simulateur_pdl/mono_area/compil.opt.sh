rmlc -runtime ctrl_tree global.rml
rmlc -runtime ctrl_tree pos_tbl.rml
rmlc -runtime ctrl_tree -s main -n 100 simul.rml
ocamlopt -c  -I `rmlc -where` global.ml
ocamlopt -c  -I `rmlc -where` pos_tbl.ml
ocamlopt -c  -I `rmlc -where` simul.ml
ocamlopt -o simul.opt -I `rmlc -where` unix.cmxa graphics.cmxa rml_interpreter.cmxa global.cmx pos_tbl.cmx simul.cmx