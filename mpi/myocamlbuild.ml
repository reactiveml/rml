open Ocamlbuild_plugin
open Command

(* Configuration section *)
let mpicc = "openmpicc"
let mpi_libs = "-L/opt/local/lib -lmpi"

let mlmpi_libdir = "-L"^Pathname.pwd^"/_build"

let static = true

let split_string s c =
  let rec aux s =
    try
      let id = String.index s c in
      let rest = String.sub s (id + 1) (String.length s - id - 1) in
      String.sub s 0 id :: aux rest
    with Not_found -> [s]
  in
  List.filter (fun s -> String.length s > 0) (aux s)

let ccopt_of_lib s =
  try
    let prefix = String.sub s 0 2 in
    if prefix = "-l" then
      [A"-cclib"; A s]
    else if prefix = "-L" then
      [A"-ccopt"; A s]
    else
      []
  with
    | _ -> []
;;

let df = function
  | After_rules ->
      let mpi_libs = split_string mpi_libs ' ' in
      let mpi_cclibs = List.flatten (List.map ccopt_of_lib mpi_libs) in
      let mpi_libs = List.map (fun x -> A x) mpi_libs in

     (* When one make a C library that use mpi with ocamlmklib,
        then issue these flags. *)
     flag ["ocamlmklib"; "c"; "use_mpi"] (S mpi_libs);

     (* When one compile C code using mpi *)
     flag ["c"; "compile"; "include_mpi"] (S[A"-cc"; A mpicc]);

     flag ["link"; "ocaml"; "library"; "use_mpi"]
          (S mpi_cclibs);

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

 | _ -> ()

let _ = dispatch df
