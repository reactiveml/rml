(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the Q Public License  *)
(*  version 1.0.                                                      *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* file: rmltop_reactive_machine.ml *)
(* created: 2007-12-03  *)
(* author: Louis Mandel *)

module I = Rml_machine.Lco_ctrl_tree_seq_interpreter.Interpreter

let make_exec_process p =
  let cd = I.R.get_top_clock_domain () in
  let result = ref None in
  let jp, join_end =
    let term_cpt = I.R.Join.new_join_point 0 in
    Some term_cpt,
    fun () ->
      I.R.Join.incr term_cpt 1;
      let f x =
        if I.R.Join.decr term_cpt then
          result := Some x
      in f
  in
  let add_process p =
    let f = p () (join_end ()) (I.R.control_tree cd) jp cd in
    I.R.on_current_instant cd f
  in
  let react pl =
    List.iter add_process pl;
    I.R.react cd
  in
  let f = p () (join_end ()) (I.R.control_tree cd) jp cd in
  I.R.on_current_instant cd f;
  react

let rml_react_unsafe =
  let react = make_exec_process Rmltop_alt_machine_body.main in
  react

let rml_react x =
  Rmltop_alt_global.lock();
  begin try rml_react_unsafe x
  with e -> Rmltop_alt_global.unlock(); raise e
  end;
  Rmltop_alt_global.unlock()
