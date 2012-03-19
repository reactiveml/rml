open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let mlmpi_dir = "../../mpi"

let df = function
  | After_rules ->
      ocaml_lib ~extern:true ~dir:mlmpi_dir "mlmpi"

  | _ -> ()

let _ = dispatch df
