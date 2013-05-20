module Rmlarg

type spec = 
| Clear of bool ref
| Float of (float -> unit)
| Int of (int -> unit)
| Rest of (string -> unit)
| Set of bool ref
| String of (string -> unit)
| Unit of (unit -> unit)

let tr_spec_list l =
  let tr_spec s = 
    match s with
    | Clear b -> Arg.Clear b
    | Float f -> Arg.Float f
    | Int f -> Arg.Int f
    | Rest f -> Arg.Rest f
    | Set b -> Arg.Set b
    | String s -> Arg.String s
    | Unit f -> Arg.Unit f 
  in
  List.map (fun (k, sp, doc) -> k, tr_spec sp, doc) l

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
    Arg.parse_argv current Sys.argv (Runtime_options.rml_cli_options @ (tr_spec_list spec_list)) anon_fun usage
  with
    | Arg.Bad s -> Printf.eprintf "%s\n" s; raise Types.End_program
    | Arg.Help s -> Printf.eprintf "%s\n" s; raise Types.End_program
