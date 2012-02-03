let bindir = ref Ocamlbuild_Myocamlbuild_config.bindir;;
let libdir = ref begin
  Filename.concat
    (try Sys.getenv "RMLLIB"
     with Not_found -> Ocamlbuild_Myocamlbuild_config.libdir)
    "rmlbuild"
end;;
