open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let mlmpi_dir = "../../../mpi"
let sdl_dir = "-L/opt/local/lib/ocaml/site-lib/sdl"

let df = function
  | After_rules ->
      (* mlmpi *)
      ocaml_lib ~extern:true ~dir:mlmpi_dir "mlmpi";

      flag["link"; "ocaml"; "byte"]
        (S[A "-cc"; A "openmpicc"; A"-ccopt"; A sdl_dir;  A"-cclib"; A "-lsdlttfstub"]);

      (* Tell ocamlbuild about the sdl library. *)
      ocaml_lib ~extern:true ~dir:"+sdl" "sdl";
      ocaml_lib ~extern:true "sdlloader";
      ocaml_lib ~extern:true "sdlgfx";
      ocaml_lib ~extern:true "sdlttf"

  | _ -> ()

let _ = dispatch df
