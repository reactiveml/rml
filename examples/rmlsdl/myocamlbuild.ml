open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let mlmpi_dir = "/Users/ccpasteur/Documents/work/git/rml/mpi/_build"
let sdl_dir = "-L/opt/local/lib/ocaml/site-lib/sdl"

let df = function
  | After_rules ->
      Myocamlbuild_config.rmlbuild_after_rules ();

      (* mlmpi *)
      ocaml_lib ~extern:true ~dir:mlmpi_dir "mlmpi";

      flag ["link"; "library"; "ocaml"; "byte"; "use_mlmpi"]
        (S[A"-dllib"; A"-lmlmpi"; A"-cclib"; A"-lmlmpi"]);

      flag ["link"; "library"; "ocaml"; "native"; "use_mlmpi"]
        (S[A"-cclib"; A"-lmlmpi"]);

     flag ["link"; "ocaml"; "library"]
          (S[A"-ccopt"; A sdl_dir;  A"-cclib"; A "-lsdlttfstub"]);

      (* Tell ocamlbuild about the sdl library. *)
      ocaml_lib ~extern:true ~dir:"+sdl" "sdl";
      ocaml_lib ~extern:true "sdlloader";
      ocaml_lib ~extern:true "sdlgfx";
      ocaml_lib ~extern:true "sdlttf"

  | _ -> ()

let _ = dispatch df
