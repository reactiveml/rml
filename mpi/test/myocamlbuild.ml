open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let mlmpi_dir = "../../../mpi"

let df = function
  | After_rules ->
      ocaml_lib ~extern:true ~dir:mlmpi_dir "mlmpi";
      flag ["with_mpicc"] (S [A "-thread"; A"-cc"; A "openmpicc"])

  | _ -> ()

let _ = dispatch df
