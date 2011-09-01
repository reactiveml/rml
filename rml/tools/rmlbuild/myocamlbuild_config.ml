open Ocamlbuild_plugin;;

dispatch begin function
  | After_rules ->
    Myocamlbuild_config.rmlbuild_after_rules ()
  | _ -> ()
end
