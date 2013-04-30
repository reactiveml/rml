module Rml_machine

open System.Diagnostics

module I = Lco
module R = Seq_runtime

let exec_forever react finalize =
  let rec exec () =
    match react () with
      | None -> exec()
      | Some v -> finalize (); Some v
  in
  exec ()

let exec_n n react finalize =
  let rec exec n =
    if n > 0 then (
      match react () with
        | None -> exec (n-1)
        | v -> finalize (); v
    ) else (
      finalize ();
      None
    )
  in
  exec n

let rml_make p =
  let timer = Stopwatch.StartNew () in
  let result = ref None in
  let cd = R.get_top_clock_domain () in
  let step = I.rml_make cd result p in
  R.on_current_instant cd step;
  let react () =
    R.react cd;
    !result
  in
  let finalize () =
    if !Runtime_options.bench_mode then
      Printf.printf "%d ms@." timer.ElapsedMilliseconds
  in
  react, finalize
