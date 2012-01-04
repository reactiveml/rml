
let doc_load_balancer = "<name> Load balancer to use (local, robin)"
let doc_number_steps = "<n> Number of steps to execute"

let number_steps = ref (- 1)
let set_number_steps i =
  number_steps := i

let errmsg = ""
let parse_cli () =
  Arg.parse
    [ "-load-balancer", Arg.String Load_balancer.set_load_balancing_policy, doc_load_balancer;
      "-n", Arg.Int set_number_steps, doc_number_steps ]
    ignore
    errmsg
