open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let df = function
  | After_rules ->
      (* Tell ocamlbuild about the sdl library. *)
      ocaml_lib ~extern:true ~dir:"+sdl" "sdl";
      ocaml_lib ~extern:true "sdlloader";
      ocaml_lib ~extern:true "sdlgfx";
      ocaml_lib ~extern:true "sdlttf";

      flag ["link"; "ocaml"; "byte"; "use_sdl"] (A"-custom")

  | _ -> ()

let _ = dispatch df
