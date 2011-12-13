open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let rmlsdl_dir = "/Users/ccpasteur/Documents/work/these/dev/rmlsdl_proc/_build"

let df = function
  | After_rules ->
      Myocamlbuild_config.rmlbuild_after_rules ();

      (* Tell ocamlbuild about the rmlsdl library. *)
      Myocamlbuild_config.rml_lib ~dir:rmlsdl_dir "rmlsdl";
      flag ["link"; "ocaml"; "byte"; "use_rmlsdl"] (S [A"-custom"; A "-I"; A "+sdl"])

  | _ -> ()

let _ = dispatch df
