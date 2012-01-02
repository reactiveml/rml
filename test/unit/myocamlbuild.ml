open Ocamlbuild_plugin;;

dispatch begin function
  | Before_options ->
      Options.use_ocamlfind := true
  | After_rules ->
    Myocamlbuild_config.rmlbuild_after_rules ()
  | _ -> ()
end
