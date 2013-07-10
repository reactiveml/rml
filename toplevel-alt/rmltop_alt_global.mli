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

(* file: rmltop_global.mli *)
(* author: Louis Mandel *)
(* created: 2005-09-23  *)

type 'a rml_process = 'a Rml_machine.Lco_ctrl_tree_seq_interpreter.Interpreter.process

val sampling : float ref

val suspend : unit option ref
val resume : unit option ref
val step : int option ref
val to_run : unit rml_process list ref

val set_suspend : unit -> unit
val set_resume : unit -> unit
val set_step : int -> unit
val set_sampling : float -> unit
val add_to_run : unit rml_process -> unit

val enter_step_by_step_mode : (unit -> unit) ref
val exit_step_by_step_mode : (unit -> unit) ref

val lock : unit -> unit
val unlock : unit -> unit
