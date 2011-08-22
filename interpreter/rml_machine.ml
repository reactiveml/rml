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

module type INTERPRETER =
  sig
    type 'a process
    module R :
     (sig
       type clock_domain
       type ('a, 'b) event
       type 'a step

       val top_clock_domain : clock_domain
       val react : clock_domain -> unit
       val on_current_instant : clock_domain -> unit step -> unit
      end)

    val rml_make: R.clock_domain -> 'a option ref -> 'a process -> unit R.step
  end

module M = functor (I : INTERPRETER) ->
  struct
    let rml_make p =
      let result = ref None in
      let step = I.rml_make I.R.top_clock_domain result p in
      I.R.on_current_instant I.R.top_clock_domain step;
      let react () =
        I.R.react I.R.top_clock_domain;
        Rmltest_utils.step ();
        !result
      in
      react

    let rml_exec p =
      let react = rml_make p in
      let rec exec () =
        Format.printf "@.@.*******************  New step ********************@.@.";
        match react () with
        | None -> exec()
        | Some v -> Rmltest_utils.end_program (); v
      in exec ()

    let rml_exec_n p n =
      let react = rml_make p in
      let rec exec n =
        if n > 0 then (
          Format.printf "@.@.*******************  New step ********************@.";
          match react () with
          | None -> exec (n-1)
          | v -> v
        ) else
          None
      in exec n

    let rml_exec_sampling p min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let react = rml_make p in
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
        | Some v -> v
      in exec ()


    let rml_exec_n_sampling p n min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let instant = ref 0 in
      let react = rml_make p in
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
      in exec n


  end

