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


type 'a rml_process = 'a Implantation.Lco_ctrl_tree_record.process

val sampling : float ref

val sampled : unit option ref
val suspend_resume : unit option ref
val step_by_step : unit option ref
val step : unit option ref
val add : unit rml_process option ref

val lock : unit -> unit
val unlock : unit -> unit

val rml_nothing : unit rml_process
val rml_halt : unit rml_process
val combine_process : unit rml_process -> unit rml_process -> unit rml_process 

val rml_react : unit -> unit
val add_process : unit rml_process -> unit