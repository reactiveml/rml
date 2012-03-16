let bindir = ref Ocamlbuild_Myocamlbuild_config.bindir;;
let libdir = ref begin
    (try   Filename.concat (Sys.getenv "RPMLLIB") "rpmlbuild"
     with Not_found -> Ocamlbuild_Myocamlbuild_config.libdir)
end;;
