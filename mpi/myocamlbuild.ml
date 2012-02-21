open Ocamlbuild_plugin
open Command

(* Configuration section *)
let mpi_lib = "-lmpi"
let mpi_libdir = "-L/opt/local/lib"
let mpi_include = "-I/opt/local/include/openmpi"

let mlmpi_libdir = "-L/Users/ccpasteur/Documents/work/git/rml/mpi/_build"

let static = true

(* List of headers *)
let headers = []

;;

let df = function
  | After_rules ->
     (* When one make a C library that use mpi with ocamlmklib,
        then issue these flags. *)
     flag ["ocamlmklib"; "c"; "use_mpi"] (S[A mpi_libdir; A mpi_lib]);

     (* When one compile C code using mpi *)
     flag ["c"; "compile"; "include_mpi"] (S[A"-ccopt"; A mpi_include]);

     flag ["link"; "ocaml"; "library"; "use_mpi"]
          (S[A"-ccopt"; A mpi_libdir; A"-cclib"; A mpi_lib]);

     (* If `static' is true then every ocaml link in bytecode will add -custom *)
     if static then flag ["link"; "ocaml"; "byte"] (A"-custom");


     ocaml_lib "mlmpi";

     flag ["link"; "library"; "ocaml"; "byte"; "use_libmlmpi"]
          (S[A"-ccopt"; A mlmpi_libdir; A"-dllib"; A"-lmlmpi"; A"-cclib"; A"-lmlmpi"]);

     flag ["link"; "library"; "ocaml"; "native"; "use_libmlmpi"]
          (S[A"-ccopt"; A mlmpi_libdir; A"-cclib"; A"-lmlmpi"]);

     (* When ocaml link something that use the libcryptokit,
        then one need that file to be up to date. *)
     dep  ["link"; "ocaml"; "use_mlmpi"] ["libmlmpi.a"];

     (* As an approximation all our C files use the headers.
        Note: This will import headers in the build directory. *)
     dep  ["compile"; "c"] headers

 | _ -> ()

let _ = dispatch df
