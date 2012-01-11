
let doc_load_balancer = "<name> Load balancer to use (local, robin)"
let doc_number_steps = "<n> Number of steps to execute"

let number_steps = ref (- 1)
let min_rank = ref 0

let errmsg = ""
let rml_cli_options =
    [ "-load-balancer", Arg.String ignore, doc_load_balancer;
      "-n", Arg.Int ignore, doc_number_steps ]

let parse_cli () =
  try
    let current = ref 1 in
    let n = Array.length Sys.argv in
      while !current < n do
        match Sys.argv.(!current) with
          | "-min-rank" ->
              incr current;
              if !current = n then
                raise (Arg.Bad ("Expected a number after -min-rank"));
              min_rank := int_of_string Sys.argv.(!current);
              incr current
          | "-load-balancer" ->
              incr current;
              if !current = n then
                raise (Arg.Bad ("Expected a load balancer name after -load-balancer"));
              Load_balancer.set_load_balancing_policy Sys.argv.(!current);
              incr current
          | "-n" ->
              incr current;
              if !current = n then
                raise (Arg.Bad ("Expected a number of steps after -n"));
              number_steps := int_of_string Sys.argv.(!current);
              incr current
          | "-help" | "--help" -> raise (Arg.Help (Arg.usage_string rml_cli_options "Usage:"))
          | _ -> incr current
      done
  with
    | Arg.Bad s -> Format.eprintf "%s@." s; exit 2
    | Arg.Help s -> Format.eprintf "%s@." s; exit 0
