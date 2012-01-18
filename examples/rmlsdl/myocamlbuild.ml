open Ocamlbuild_plugin
open Ocamlbuild_plugin.Options

let df = function
 | Before_options ->
      Options.use_ocamlfind := true

  | After_rules ->
      Myocamlbuild_config.rmlbuild_after_rules ();

      (* Tell ocamlbuild about the sdl library. *)
      ocaml_lib ~extern:true ~dir:"+sdl" "sdl";
      ocaml_lib ~extern:true "sdlloader";
      ocaml_lib ~extern:true "sdlgfx";
      ocaml_lib ~extern:true "sdlttf";

      flag ["link"; "ocaml"; "byte"; "use_sdl"] (A"-custom");
      flag ["link"; "ocaml"] (S[A"-cclib"; A "-lmpl"])

  | _ -> ()

let _ = dispatch df
