
include Arg

let parse spec_list anon_fun usage =
  try
    Arg.parse_argv Sys.argv (Runtime_options.rml_cli_options @ spec_list) anon_fun usage
  with
    | Arg.Bad s -> Format.eprintf "%s@." s; raise Types.RML
    | Arg.Help s -> Format.eprintf "%s@." s; raise Types.RML
