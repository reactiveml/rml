(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library       *)
(*  General Public License, with the special exception on linking     *)
(*  described in file ../LICENSE.                                     *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* author: Louis Mandel *)
(* created: 2005-09-13  *)
(* file: rml_machine.ml *)

open Runtime_options

module type MACHINE_INTERPRETER = sig
  type 'a process

  val rml_make : 'a process -> (unit -> 'a option) * (unit -> unit)
  val rml_make_test : (((int -> unit) -> 'a process) * string * int Rmltest.behaviour list) list ->
    (unit -> 'a option) * (unit -> unit)
end

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

let exec_sampling_forever min react finalize =
  let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
  let debut = ref 0.0 in
  let fin = ref 0.0 in
  let diff = ref 0.0 in
  let rec exec () =
    let _ = debut := Sys.time() in
    let v = react () in
    let _ =
      fin := Sys.time();
      diff := min -. (!fin -. !debut);
      if !diff > 0.001 then (
        ignore (Unix.setitimer
                   Unix.ITIMER_REAL
                   {Unix.it_interval = 0.0; Unix.it_value = !diff});
        Unix.pause())
      else ();
    in
    match v with
      | None -> exec ()
      | Some v -> finalize (); Some v
  in
  exec ()

let exec_sampling_n n min react finalize =
  let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
  let debut = ref 0.0 in
  let fin = ref 0.0 in
  let diff = ref 0.0 in
  let instant = ref 0 in
  let rec exec n =
    if n > 0 then
      let _ =
        print_string ("************ Instant "^
                         (string_of_int !instant)^
                         " ************");
        print_newline();
        debut := Sys.time();
        incr instant
      in
      let _ = debut := Sys.time() in
      let v = react () in
      let _ =
        fin := Sys.time();
        diff := min -. (!fin -. !debut);
        if !diff > 0.001 then (
          ignore (Unix.setitimer
                     Unix.ITIMER_REAL
                     {Unix.it_interval = 0.0; Unix.it_value = !diff});
          Unix.pause())
        else
          (print_string "Instant ";
           print_int !instant;
           print_string " : depassement = ";
           print_float (-. !diff);
           print_newline());
      in
      match v with
        | None -> exec (n-1)
        | v -> v
    else
    None
  in
  exec n

module M (I : MACHINE_INTERPRETER) =
  struct
    let rml_exec p =
      Runtime_options.parse_cli ();
      let react, finalize = I.rml_make p in
      let react_fun =
        match !Runtime_options.number_steps > 0, !Runtime_options.sampling_rate > 0.0 with
          | false, false -> exec_forever
          | true, false -> exec_n !Runtime_options.number_steps
          | false, true -> exec_sampling_forever !Runtime_options.sampling_rate
          | true, true -> exec_sampling_n !Runtime_options.number_steps !Runtime_options.sampling_rate
      in
      try
        react_fun react finalize
      with
        | e -> Format.eprintf "An exeception occurred: %s.@.Aborting all processes@." (Printexc.to_string e); finalize (); exit 2

    let rml_test test_list =
      Runtime_options.parse_cli ();
      let react, finalize = I.rml_make_test test_list in
      try
        exec_forever react finalize
      with
        | Rmltest.Test_success -> finalize (); exit 0
        | Rmltest.Test_failed _ -> finalize (); exit 2
        | _ -> Format.eprintf "Error: An exception was raised. Aborting@."; finalize (); exit 2
  end



module type INTERPRETER =
  sig
    type 'a process
    type ('a, 'b) event

    module R :
     (sig
       type clock_domain
       type ('a, 'b) event
       type 'a step

       val mk_top_clock_domain : unit -> clock_domain
       val react : clock_domain -> unit
       val on_current_instant : clock_domain -> unit step -> unit
      end)

    val rml_make: R.clock_domain -> 'a option ref -> 'a process -> unit R.step
    val rml_make_n: R.clock_domain -> 'a option ref -> 'a process list -> unit R.step list
  end

module Machine = functor (I : INTERPRETER) ->
struct
  module T = Rmltest.Test

  module MyInterpreter = struct
    type 'a process = 'a I.process

    let rml_make p =
      let start_t = Unix.gettimeofday () in
      let result = ref None in
      let cd = I.R.mk_top_clock_domain () in
      let step = I.rml_make cd result p in
      I.R.on_current_instant cd step;
      let react () =
        I.R.react cd;
        !result
      in
      let finalize () =
        if !Runtime_options.bench_mode then
          let end_t = Unix.gettimeofday () in
          Format.printf "%f@." (end_t -. start_t)
      in
      react, finalize

    let rml_make_test test_list =
      let result = ref None in
      let cd = I.R.mk_top_clock_domain () in
      let mk_test (p, name, expected) =
        let act = T.new_test name expected in
        p act
      in
      let pl = List.map mk_test test_list in
      let steps = I.rml_make_n cd result pl in
      List.iter (fun step -> I.R.on_current_instant cd step) steps;
      let react () =
        I.R.react cd;
        T.next_step ();
        !result
      in
      let finalize () = T.end_test () in
      react, finalize
  end

  include M(MyInterpreter)
end

module Lco_ctrl_tree_seq_interpreter =
  Lco_ctrl_tree_n.Rml_interpreter(Seq_runtime.SeqRuntime)
module Lco_ctrl_tree_seq_list_interpreter =
  Lco_ctrl_tree_n.Rml_interpreter(Seq_runtime.SeqListRuntime)
