
open Rmllib
open Implem_lco_ctrl_tree_record
let run p =
  Rml_machine.rml_exec ([])
    (fun () -> Lco_ctrl_tree_record.rml_run (function | ()  -> p ) )

let compute_n_seq n =
  let rec fac n = if n < 2 then 1 else n * fac (n - 1) in 
  let n2 = ((n + 1) * n) / 2 in
  Await.ratio * Await.ratio * n2 + Await.ratio * (fac n) + (2 * n2)
  
  let compute_n n = run (Await.compare n)
  
  let test_await () =
  Alcotest.(check int) "await_when 1" (compute_n_seq 10) (compute_n 10)
  
  let test_set = [
  ("await_when", `Quick, test_await)
  ]