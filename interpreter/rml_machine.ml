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

module LcoInterpreter(I:Lco_interpreter.S) =
struct
  type 'a process = 'a I.process

  let rml_make p =
    let result = ref None in
    let step = I.rml_make I.R.top_clock_domain result p in
    (*R.init ();*)
    I.R.on_current_instant I.R.top_clock_domain step;
    let react () =
      I.R.react I.R.top_clock_domain;
      !result
    in
    react
end

module LcoSeqInterpreter = LcoInterpreter(Lco_ctrl_tree_n.Lco_ctrl_tree_seq_interpreter)

module type Interpretor_type =
  sig
    type 'a process
    val rml_make: 'a process -> (unit -> 'a option)
  end

module M =
  functor (Interpretor: Interpretor_type) ->
  struct

    let rml_exec p =
      let react = Interpretor.rml_make p in
      let rec exec () =
        match react () with
        | None -> Format.printf "@.@.New step@."; exec()
        | Some v -> v
      in exec ()

    let rml_exec_n p n =
      let react = Interpretor.rml_make p in
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
      let react = Interpretor.rml_make p in
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
      let react = Interpretor.rml_make p in
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

