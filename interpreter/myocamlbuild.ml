open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let df = function
  | Before_options ->
      Options.use_ocamlfind := true
  | _ -> ()

let _ = dispatch df
