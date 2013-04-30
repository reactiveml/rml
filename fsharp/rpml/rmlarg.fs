module Rmlarg

type spec = Arg.spec

let is_reserved_arg s =
  let rec aux l = match l with
    | [] -> false
    | (n, _, _)::l -> n = s or (aux l)
  in
  aux Runtime_options.rml_cli_options

let check_reserved_args spec_list =
  let filter_spec (n, _, _) =
    if is_reserved_arg n then (
      Printf.eprintf
        "Warning: The argument '%s' is used by ReactiveML runtime.\nPlease rename this option.@." n;
      false
    ) else
      true
  in
  List.filter filter_spec spec_list

let parse spec_list anon_fun usage =
  try
    let spec_list = check_reserved_args spec_list in
    let current = ref 0 in
    Arg.parse_argv current Sys.argv (Runtime_options.rml_cli_options @ spec_list) anon_fun usage
  with
    | Arg.Bad s -> Printf.eprintf "%s\n" s; raise Types.End_program
    | Arg.Help s -> Printf.eprintf "%s\n" s; raise Types.End_program
