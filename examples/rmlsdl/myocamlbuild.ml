open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let df = function
  | After_rules ->
      (* there is a bug in ocamlbuild 3.12.0 with ocamlfind that doesn't pass the
         -thread option to ocamlbuild *)
      flag ["ocaml"; "program"] (A "-thread");
      flag ["ocaml"; "library"] (A "-thread")

  | _ -> ()

let _ = dispatch df
