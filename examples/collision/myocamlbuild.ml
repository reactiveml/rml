open Ocamlbuild_plugin

let rmlsdl_dir = "../../rmlsdl/_build"

let df = function
  | After_rules ->
      (* Tell ocamlbuild about the rmlsdl library. *)
      rml_lib ~dir:rmlsdl_dir "rmlsdl"

  | _ -> ()

let _ = dispatch df
