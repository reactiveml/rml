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

module type MACHINE_INTERPRETER = sig
  type 'a process

  val rml_make : 'a process -> (unit -> 'a option) * (unit -> unit)
  val rml_make_test : 'a process list -> (unit -> 'a option) * (unit -> unit)
end

module M = functor (I : MACHINE_INTERPRETER) ->
  struct
    let rml_exec p =
      Runtime_options.parse_cli ();
      let react, finalize = I.rml_make p in
      let rec exec () =
        match react () with
        | None -> exec()
        | Some v -> finalize (); v
      in exec ()

    let rml_exec_n p n =
      Runtime_options.parse_cli ();
      let react, finalize = I.rml_make p in
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
      let n = if !Runtime_options.number_steps = -1 then n else !Runtime_options.number_steps in
      exec n

    let rml_exec_sampling p min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let react, finalize = I.rml_make p in
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
        | Some v -> finalize (); v
      in exec ()


    let rml_exec_n_sampling p n min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let instant = ref 0 in
      let react, finalize = I.rml_make p in
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
          | v -> finalize (); v
        else
          None
      in exec n

    let rml_test test_list =
      let mk_test (p, name, expected) =
        let act, n = Rmltest.mk_checker name expected in
        p act
      in
      let pl = List.map mk_test test_list in
      let react, finalize = I.rml_make_test pl in
      let rec exec () =
        Format.printf "@.@.*******************  New step ********************@.@.";
        match react () with
        | None -> exec()
        | Some v -> Rmltest.end_program (); finalize (); v
      in exec ()

  end



module type SEQ_INTERPRETER =
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

module Seq = functor (I : SEQ_INTERPRETER) ->
struct

  module MyInterpreter = struct
    type 'a process = 'a I.process

    let rml_make p =
      let result = ref None in
      let cd = I.R.mk_top_clock_domain () in
      let step = I.rml_make cd result p in
      I.R.on_current_instant cd step;
      let react () =
        I.R.react cd;
        !result
      in
      let finalize () = () in
      react, finalize

    let rml_make_test pl =
      let result = ref None in
      let cd = I.R.mk_top_clock_domain () in
      let steps = I.rml_make_n cd result pl in
      List.iter (fun step -> I.R.on_current_instant cd step) steps;
      let react () =
        I.R.react cd;
        Rmltest.step ();
        !result
      in
      let finalize () = () in
      react, finalize
  end

  include M(MyInterpreter)
end


module type DISTRIBUTED_INTERPRETER =
  sig
    type 'a process
    type ('a, 'b) event

    module R : (sig
      type clock_domain
      type ('a, 'b) event
      type 'a step

      val mk_top_clock_domain : unit -> clock_domain
      val finalize_top_clock_domain : clock_domain -> unit
      val react : clock_domain -> unit
      val on_current_instant : clock_domain -> unit step -> unit

      val is_master : unit -> bool
      val start_slave : unit -> unit
    end)

    val rml_make: R.clock_domain -> 'a option ref -> 'a process -> unit R.step
    val rml_make_n: R.clock_domain -> 'a option ref -> 'a process list -> unit R.step list
  end

module Distributed = functor (I : DISTRIBUTED_INTERPRETER) ->
struct
  module MyInterpreter = struct
    type 'a process = 'a I.process

    let rml_make p =
      if I.R.is_master () then (
        Thread.delay 1.0;
        let result = ref None in
        let cd = I.R.mk_top_clock_domain () in
        let step = I.rml_make cd result p in
        I.R.on_current_instant cd step;
        let react () =
          Format.eprintf "@.Doing one step@.";
          I.R.react cd;
          !result
        in
        let finalize () =
          I.R.finalize_top_clock_domain cd
        in
        react, finalize
      ) else
        (fun _ -> Format.eprintf "Launching slave@."; I.R.start_slave (); None), (fun () -> ())

    let rml_make_test pl =
      if I.R.is_master () then (
        let result = ref None in
        let cd = I.R.mk_top_clock_domain () in
        let steps = I.rml_make_n cd result pl in
        List.iter (fun step -> I.R.on_current_instant cd step) steps;
        let react () =
          I.R.react cd;
          Rmltest.step ();
          !result
        in
        let finalize () =
          I.R.finalize_top_clock_domain cd
        in
        react, finalize
      ) else
        (fun _ -> I.R.start_slave (); None), (fun () -> ())

  end

  include M(MyInterpreter)
end

