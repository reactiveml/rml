open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let df = function
  | Before_options ->
      Options.use_ocamlfind := true

  | After_rules ->
      flag ["link"; "ocaml"] (S[A"-cclib"; A "-lmpl"])

  | _ -> ()

let _ = dispatch df
