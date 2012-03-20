open Ocamlbuild_plugin

let rmlsdl_dir = "../../rmlsdl/_build"

let df = function
  | After_rules ->
      (* Tell ocamlbuild about the rmlsdl library. *)
      rml_lib ~dir:rmlsdl_dir ~native:true "rmlsdl";
      (* there is a bug in ocamlbuild 3.12.0 with ocamlfind that doesn't pass the
         -thread option to ocamlbuild *)
      flag ["ocaml"; "program"] (A "-thread")

  | _ -> ()

let _ = dispatch df
