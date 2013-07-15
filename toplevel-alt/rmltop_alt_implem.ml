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

(* file: rmltop_implem.ml *)
(* author: Louis Mandel *)
(* created: 2005-10-25  *)

module SeqRuntime = Rmltop_interpreter.Seq_runtime.SeqRuntime

module Interpreter = Rmltop_interpreter.Lco_ctrl_tree_n.Rml_interpreter(SeqRuntime)

module Rml_types = Rmltop_interpreter.Rml_types

let make p =
  let cd = SeqRuntime.get_top_clock_domain () in
  let result = ref None in
  let step = Interpreter.rml_make cd result p in
  SeqRuntime.on_current_instant cd step;
  fun () ->
    SeqRuntime.react cd;
    !result
