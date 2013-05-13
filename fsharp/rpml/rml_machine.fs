module Machine

open System.Diagnostics
open Runtime

(*
type IRuntime<'cd> = 
  abstract member init : unit -> unit
  abstract member finalize_top_clock_domain : unit -> unit
  abstract member get_top_clock_domain : unit -> 'cd
  abstract member react : 'cd -> unit
  abstract member on_current_instant : 'cd -> (unit -> unit) -> unit
*)

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

type Machine<'ck, 'ctrl>(R:Runtime<'ck, 'ctrl>) =
    member this.make_react_finalize rml_make p =
      let timer = Stopwatch.StartNew () in
      let result = ref None in
      let cd = R.get_top_clock_domain () in
      let step = rml_make cd result p in
      R.on_current_instant cd step;
      let react () =
        R.react cd;
        !result
      in
      let finalize () =
        R.finalize_top_clock_domain cd;
        if !Runtime_options.bench_mode then
          Printf.printf "%d ms@." timer.ElapsedMilliseconds
      in
      react, finalize
      
    member this.rml_exec rml_make p =
      Runtime_options.parse_cli (); R.init();
      let react, finalize = this.make_react_finalize rml_make p in
      let react_fun =
        match !Runtime_options.number_steps > 0, !Runtime_options.sampling_rate > 0.0 with
          | false, false -> exec_forever
          | true, false -> exec_n !Runtime_options.number_steps
          | _ -> raise Types.RML
          (*| false, true -> exec_sampling_forever !Runtime_options.sampling_rate
          | true, true -> exec_sampling_n !Runtime_options.number_steps !Runtime_options.sampling_rate*)
      in
      try
        react_fun react finalize
      with
        | Types.End_program -> finalize (); exit 0
        | e ->
            Printf.eprintf "Error: An exception occurred: %s.@.Aborting all processes@."
              (Printexc.to_string e);
            finalize ();
            exit 2

let LcoSeq = new Lco.Interpreter<_,_>(SeqRuntime.R)
let SeqMachine = Machine<_,_>(SeqRuntime.R :> Runtime<_,_>)

let LcoRmlThread = new Lco.Interpreter<_,_>(RmlThreadRuntime.R)
let RmlThreadMachine = Machine<_,_>(RmlThreadRuntime.R :> Runtime<_,_>)

let LcoThread = new Lco.Interpreter<_,_>(ThreadRuntime.R)
let ThreadMachine = Machine<_,_>(ThreadRuntime.R :> Runtime<_,_>)
