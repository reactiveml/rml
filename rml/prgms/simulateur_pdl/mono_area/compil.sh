rmlc -runtime ctrl_tree global.rml
rmlc -runtime ctrl_tree pos_tbl.rml
rmlc -runtime ctrl_tree -s main -n 100 simul.rml
ocamlc -c  -I `rmlc -where` global.ml
ocamlc -c  -I `rmlc -where` pos_tbl.ml
ocamlc -c  -I `rmlc -where` simul.ml
ocamlc -o simul -I `rmlc -where` unix.cma graphics.cma rml_interpreter.cma global.cmo pos_tbl.cmo simul.cmo