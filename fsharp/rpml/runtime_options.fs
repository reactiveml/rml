module Runtime_options

let doc_number_steps = "<n> Number of steps to execute"
let doc_sampling = "<f> Minimum step period"
let doc_debug = "Print debugging information"
let doc_bench = "Print the elapsed time on the standard output"

let number_steps = ref (- 1)
let sampling_rate = ref (-1.0)
let min_rank = ref 0
let debug_mode = ref false
let bench_mode = ref false

let errmsg = ""
let rml_cli_options =
    [ "-n", Arg.Int ignore, doc_number_steps;
      "-sampling", Arg.Float ignore, doc_sampling;
      "-debug", Arg.Unit ignore, doc_debug;
      "-bench", Arg.Unit ignore, doc_bench;
    ]

let _parse_cli () =
  try
    let current = ref 1 in
    let n = Array.length Sys.argv in
      while !current < n do
        let arg = Sys.argv.(!current) in
        incr current;
        match arg with
          | "-bench" -> bench_mode := true
          | "-debug" -> debug_mode := true
          | "-n" ->
              if !current = n then
                raise (Arg.Bad ("Expected a number of steps after -n"));
              number_steps := int_of_string Sys.argv.(!current);
              incr current
          | "-sampling" ->
              if !current = n then
                raise (Arg.Bad ("Expected a period after -sampling"));
              sampling_rate := float_of_string Sys.argv.(!current);
              incr current
          (*| "-help" | "--help" -> raise (Arg.Help (Arg.usage_string rml_cli_options "Usage:")) *)
          | _ -> ()
      done
  with
    | Arg.Bad s -> Printf.eprintf "%s@." s; exit 2
    | Arg.Help s -> Printf.eprintf "%s@." s; exit 0

let parsing_done = ref false
let parse_cli () =
  if not !parsing_done then (
    _parse_cli ();
    parsing_done := true
  )

(*
let print_debug fmt =
  if !debug_mode then
    Format.eprintf fmt
  else
    Format.ifprintf Format.err_formatter fmt
    *)

