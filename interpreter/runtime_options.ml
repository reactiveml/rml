
let doc_load_balancer = "<name> Load balancer to use (local, robin)"
let doc_number_steps = "<n> Number of steps to execute"
let doc_sampling = "<f> Minimum step period"
let doc_min_rank = "<n> Use only ranks starting from n (only for MPI backend)"
let doc_debug = "Print debugging information"
let doc_bench = "Print the elapsed time on the standard output"
let doc_signals_remote = "Do not use a remote set for each signal"
let doc_local_slow_signals = "Do not allocate slow local signals"

let number_steps = ref (- 1)
let sampling_rate = ref (-1.0)
let min_rank = ref 0
let debug_mode = ref false
let bench_mode = ref false

let use_signals_users_set = ref true
let use_local_slow_signals = ref true

type policy = Plocal | Pround_robin | Puser_local | Puser_robin | Premote
let load_balancing_policy = ref Pround_robin

let set_load_balancing_policy s =
  let p =
    match s with
      | "local" -> Plocal
      | "robin" -> Pround_robin
      | "user_local" -> Puser_local
      | "user_robin" -> Puser_robin
      | "remote" -> Premote
      | _ -> raise (Arg.Bad ("Invalid load balancing policy"))
  in
  load_balancing_policy := p

let errmsg = ""
let rml_cli_options =
    [ "-load-balancer", Arg.String ignore, doc_load_balancer;
      "-n", Arg.Int ignore, doc_number_steps;
      "-sampling", Arg.Float ignore, doc_sampling;
      "-min-rank", Arg.Int ignore, doc_min_rank;
      "-debug", Arg.Unit ignore, doc_debug;
      "-bench", Arg.Unit ignore, doc_bench;
      "-no-signals-remote", Arg.Unit ignore, doc_signals_remote;
      "-no-local-slow-signals", Arg.Unit ignore, doc_local_slow_signals;
    ]

let parse_cli () =
  try
    let current = ref 1 in
    let n = Array.length Sys.argv in
      while !current < n do
        let arg = Sys.argv.(!current) in
        incr current;
        match arg with
          | "-no-signals-remote" -> use_signals_users_set := false
          | "-no-local-slow-signals" -> use_local_slow_signals := false
          | "-bench" -> bench_mode := true
          | "-debug" -> debug_mode := true
          | "-min-rank" ->
              if !current = n then
                raise (Arg.Bad ("Expected a number after -min-rank"));
              min_rank := int_of_string Sys.argv.(!current);
              incr current
          | "-load-balancer" ->
              if !current = n then
                raise (Arg.Bad ("Expected a load balancer name after -load-balancer"));
              set_load_balancing_policy Sys.argv.(!current);
              incr current
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
    | Arg.Bad s -> Format.eprintf "%s@." s; exit 2
    | Arg.Help s -> Format.eprintf "%s@." s; exit 0

let parsing_done = ref false
let parse_cli () =
  if not !parsing_done then (
    parse_cli ();
    parsing_done := true
  )

let print_debug fmt =
  if !debug_mode then
    Format.eprintf fmt
  else
    Format.ifprintf Format.err_formatter fmt

