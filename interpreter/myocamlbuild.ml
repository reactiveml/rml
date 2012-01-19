open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let mlmpi_dir = "/Users/ccpasteur/Documents/work/git/rml/mpi/_build"

let df = function
  | After_rules ->
      ocaml_lib ~extern:true ~dir:mlmpi_dir "mlmpi";

      flag ["link"; "library"; "ocaml"; "byte"; "use_mlmpi"]
        (S[A"-dllib"; A"-lmlmpi"; A"-cclib"; A"-lmlmpi"]);

      flag ["link"; "library"; "ocaml"; "native"; "use_mlmpi"]
        (S[A"-cclib"; A"-lmlmpi"])

  | _ -> ()

let _ = dispatch df
